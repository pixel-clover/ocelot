{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | SDL2-backed frontend with video and audio.

This module is the only place in the project that depends on @sdl2@. It opens a window, opens
a 48 kHz stereo audio device with a callback that drains an 'MVar'-shared sample buffer, and runs
the emulator one frame at a time.

The library is unaware of SDL: the only library calls used here are

* 'machineFromCartridge', 'runFor', 'machineBus', 'busPpu', 'busJoypad'
* 'Ppu.framebuffer'
* 'Joypad.setButton'
* 'Bus.drainAudioSamples'

so this frontend is interchangeable with the headless terminal mode.
-}
module Frontend.Sdl (
    play,
    audioTest,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64, Word8)
import Foreign.C.Types (CInt)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Ocelot.Apu as Apu
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge)
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Joypad (Button (..), JoypadState)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Machine (Machine (..), machineFromCartridgeWithBoot)
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot as Snap
import SDL (($=))
import qualified SDL
import qualified SDL.Input.GameController as SDLGC

gbWidth, gbHeight :: Int
gbWidth = 160
gbHeight = 144

windowSize :: Int -> (Int, Int)
windowSize s = (gbWidth * s, gbHeight * s)

frameNs :: Word64
frameNs = 16742706

-- | Analog stick deadzone (~25 % of the 32 767 maximum).
axisDeadzone :: Int16
axisDeadzone = 8000

-- | Cap the audio buffer so an emulator running ahead of the audio device doesn't accumulate unbounded samples.
maxBufferedSamples :: Int
maxBufferedSamples = 9600 -- ~100 ms of stereo samples at 48 kHz

data Hotkeys = Hotkeys
    { hkQuit :: !(IORef Bool)
    , hkPaused :: !(IORef Bool)
    , hkFastFwd :: !(IORef Bool)
    , hkSaveReq :: !(IORef Bool)
    , hkLoadReq :: !(IORef Bool)
    , hkShotReq :: !(IORef Bool)
    , hkFrameStepReq :: !(IORef Bool)
    -- ^ "." while paused: run one frame, then re-pause.
    , hkResetReq :: !(IORef Bool)
    -- ^ "R": rebuild the Machine from the cartridge.
    }

newHotkeys :: IO Hotkeys
newHotkeys =
    Hotkeys
        <$> newIORef False
        <*> newIORef False
        <*> newIORef False
        <*> newIORef False
        <*> newIORef False
        <*> newIORef False
        <*> newIORef False
        <*> newIORef False

data ToastStyle
    = ToastInfo
    | ToastSuccess
    | ToastFailure
    deriving (Eq, Show)

data Toast = Toast
    { toastStyle :: !ToastStyle
    , toastMessage :: !String
    , toastHideAfterFrame :: !Word64
    }

data UiState = UiState
    { uiHelpVisible :: !(IORef Bool)
    , uiFrameCounter :: !(IORef Word64)
    , uiToasts :: !(IORef [Toast])
    }

newUiState :: IO UiState
newUiState =
    UiState
        <$> newIORef False
        <*> newIORef 0
        <*> newIORef []

tickUi :: UiState -> IO Word64
tickUi ui = do
    frame0 <- readIORef (uiFrameCounter ui)
    let frame = frame0 + 1
    writeIORef (uiFrameCounter ui) frame
    modifyIORef' (uiToasts ui) (filter (\toast -> toastHideAfterFrame toast > frame))
    pure frame

pushToast :: UiState -> ToastStyle -> String -> IO ()
pushToast ui style message = do
    frame <- readIORef (uiFrameCounter ui)
    let toast =
            Toast
                { toastStyle = style
                , toastMessage = message
                , toastHideAfterFrame = frame + 240
                }
    modifyIORef'
        (uiToasts ui)
        ( \toasts ->
            let active = filter (\entry -> toastHideAfterFrame entry > frame) toasts
             in if length active < 3
                    then active ++ [toast]
                    else take 2 active ++ [toast]
        )

currentToast :: UiState -> IO (Maybe Toast)
currentToast ui = listToMaybe <$> readIORef (uiToasts ui)

fallbackTitle :: FilePath -> String
fallbackTitle path =
    let fileName = reverse (takeWhile (/= '/') (reverse path))
        stem = reverse (drop 1 (dropWhile (/= '.') (reverse fileName)))
     in if null stem then fileName else stem

play :: FilePath -> Cartridge -> Maybe BS.ByteString -> Text -> Int -> IO ()
play romPath cart bootRom title scale = do
    let titleStr
            | T.null title = fallbackTitle romPath
            | otherwise = T.unpack title
        (winW, winH) = windowSize scale
    SDL.initialize
        [ SDL.InitVideo
        , SDL.InitEvents
        , SDL.InitAudio
        , SDL.InitGameController
        ]
    -- Open the first connected controller, if any. Disconnect events later are not specially handled;
    -- SDL still posts ControllerButton events.
    controllers <- SDLGC.availableControllers
    _maybeController <- case (toList controllers, ()) of
        (dev : _, _) -> Just <$> SDLGC.openController dev
        _ -> pure Nothing
    window <-
        SDL.createWindow
            ("Ocelot - " <> title)
            SDL.defaultWindow
                { SDL.windowInitialSize =
                    SDL.V2 (fromIntegral winW) (fromIntegral winH)
                }
    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedRenderer}
    SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
    texture <-
        SDL.createTexture
            renderer
            SDL.RGB24
            SDL.TextureAccessStreaming
            (SDL.V2 (fromIntegral gbWidth) (fromIntegral gbHeight))

    audioBuf <- newMVar []
    audioDev <- openAudio audioBuf

    machine0 <- machineFromCartridgeWithBoot bootRom cart
    machineRef <- newIORef machine0
    hk <- newHotkeys
    ui <- newUiState

    SDL.setAudioDevicePlaybackState audioDev SDL.Play

    loop romPath titleStr hk ui machineRef cart bootRom renderer texture audioBuf winW winH

    SDL.setAudioDevicePlaybackState audioDev SDL.Pause
    SDL.closeAudioDevice audioDev
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

openAudio :: MVar [Int16] -> IO SDL.AudioDevice
openAudio buf = do
    let spec =
            SDL.OpenDeviceSpec
                { SDL.openDeviceFreq = SDL.Desire (fromIntegral Apu.sampleRate)
                , SDL.openDeviceFormat = SDL.Desire SDL.Signed16BitNativeAudio
                , SDL.openDeviceChannels = SDL.Desire SDL.Stereo
                , SDL.openDeviceSamples = 1024
                , SDL.openDeviceCallback = audioCallback buf
                , SDL.openDeviceUsage = SDL.ForPlayback
                , SDL.openDeviceName = Nothing
                }
    (dev, _actualSpec) <- SDL.openAudioDevice spec
    pure dev

{- | Audio callback, invoked by SDL's audio thread when it needs more samples.
Drains up to @VSM.length out@ samples from the shared buffer, padding with silence if the producer is behind.
-}
audioCallback ::
    forall sampleType.
    MVar [Int16] ->
    SDL.AudioFormat sampleType ->
    VSM.IOVector sampleType ->
    IO ()
audioCallback buf fmt out = case fmt of
    SDL.Signed16BitNativeAudio -> writeInt16 buf out
    SDL.Signed16BitLEAudio -> writeInt16 buf out
    SDL.Signed16BitBEAudio -> writeInt16 buf out
    _ -> pure ()

writeInt16 :: MVar [Int16] -> VSM.IOVector Int16 -> IO ()
writeInt16 buf out = do
    let !needed = VSM.length out
    samples <-
        modifyMVar
            buf
            ( \xs -> do
                let (taken, rest) = splitAt needed xs
                    padded = taken ++ replicate (needed - length taken) 0
                pure (rest, padded)
            )
    mapM_ (uncurry (VSM.write out)) (zip [0 ..] samples)

loop ::
    FilePath ->
    String ->
    Hotkeys ->
    UiState ->
    IORef Machine ->
    Cartridge ->
    Maybe BS.ByteString ->
    SDL.Renderer ->
    SDL.Texture ->
    MVar [Int16] ->
    Int ->
    Int ->
    IO ()
loop romPath titleStr hk ui machineRef cart bootRom renderer texture audioBuf winW winH = do
    quit <- readIORef (hkQuit hk)
    unless quit $ do
        _ <- tickUi ui
        machine <- readIORef machineRef
        events <- SDL.pollEvents
        mapM_ (handleEvent hk ui (Bus.busJoypad (machineBus machine))) events

        -- One-shot hotkeys: handle save/load/screenshot/reset requests.
        handlePending romPath hk ui machineRef cart bootRom

        -- Re-read in case reset swapped the machine.
        machine' <- readIORef machineRef
        paused <- readIORef (hkPaused hk)
        helpVisible <- readIORef (uiHelpVisible ui)
        fast <- readIORef (hkFastFwd hk)
        stepOnce <- readIORef (hkFrameStepReq hk)
        when stepOnce (writeIORef (hkFrameStepReq hk) False)
        let frames = if fast then 4 else 1 :: Int
            shouldRun = (not paused && not helpVisible) || (stepOnce && paused && not helpVisible)

        frameStartNs <- getMonotonicTimeNSec
        when shouldRun $ do
            mapM_
                ( \_ -> do
                    frameCycles <- Bus.cpuMCyclesPerLcdFrame (machineBus machine')
                    _ <- runFor frameCycles machine'
                    pure ()
                )
                [1 .. frames]

            samples <- Bus.drainAudioSamples (machineBus machine')
            unless (null samples) $
                modifyMVar_
                    audioBuf
                    ( \existing -> do
                        let !combined = existing ++ samples
                            !trimmed =
                                if length combined > maxBufferedSamples
                                    then drop (length combined - maxBufferedSamples) combined
                                    else combined
                        pure trimmed
                    )

        fbRgb <- Ppu.framebufferRgb (Bus.busPpu (machineBus machine'))
        updateTextureRgb texture fbRgb
        SDL.clear renderer
        SDL.copy renderer texture Nothing Nothing
        renderUi renderer ui titleStr machine' paused fast winW winH
        SDL.present renderer

        unless fast (paceFrame frameStartNs)
        loop romPath titleStr hk ui machineRef cart bootRom renderer texture audioBuf winW winH

paceFrame :: Word64 -> IO ()
paceFrame frameStartNs = do
    now <- getMonotonicTimeNSec
    let elapsedNs = now - frameStartNs
    when (elapsedNs < frameNs) $
        threadDelay
            ( fromIntegral $
                (frameNs - elapsedNs + 999) `div` 1000
            )

panelPrimary, panelSecondary, panelOverlay, accentOrange, accentBlue :: SDL.V4 Word8
panelPrimary = SDL.V4 0x0D 0x11 0x17 0xE8
panelSecondary = SDL.V4 0x0E 0x14 0x19 0xEE
panelOverlay = SDL.V4 0x0F 0x13 0x18 0xD8
accentOrange = SDL.V4 0xF7 0xA4 0x1D 0xFF
accentBlue = SDL.V4 0x5B 0x8D 0xBE 0xFF

textPrimary, textSecondary, textMuted, shadowColor :: SDL.V4 Word8
textPrimary = SDL.V4 0xE6 0xED 0xF3 0xFF
textSecondary = SDL.V4 0x8B 0x94 0x9E 0xFF
textMuted = SDL.V4 0xC7 0xD2 0xE0 0xFF
shadowColor = SDL.V4 0x00 0x00 0x00 0x99

successFill, failureFill, successAccent, failureAccent :: SDL.V4 Word8
successFill = SDL.V4 0x0D 0x18 0x12 0xE8
failureFill = SDL.V4 0x1B 0x0F 0x11 0xEC
successAccent = SDL.V4 0x89 0xDA 0xA2 0xFF
failureAccent = SDL.V4 0xFF 0x9B 0x8E 0xFF

data UiSection = UiSection
    { sectionHeader :: !String
    , sectionAccent :: !(SDL.V4 Word8)
    , sectionRows :: ![String]
    }

renderUi :: SDL.Renderer -> UiState -> String -> Machine -> Bool -> Bool -> Int -> Int -> IO ()
renderUi renderer ui title machine paused fast winW winH = do
    helpVisible <- readIORef (uiHelpVisible ui)
    toast <- currentToast ui
    let bus = machineBus machine
        platformLabel = if Bus.busCgb bus then "CGB" else "DMG"
    doubleSpeed <- readIORef (Bus.busDoubleSpeed bus)
    let speedLabel = if doubleSpeed then "DOUBLE SPEED" else "NORMAL SPEED"
        clippedTitle = fitText 28 title
        overlayVisible = paused || helpVisible

    when overlayVisible $ do
        fillRect renderer (SDL.V4 0x00 0x00 0x00 0x98) 0 0 winW winH
        renderOverlayHeader renderer winW (if helpVisible then "HELP" else "PAUSED")
        if helpVisible
            then renderHelpOverlay renderer winW clippedTitle platformLabel speedLabel fast
            else renderPauseOverlay renderer winW clippedTitle platformLabel speedLabel fast
        renderStatusBar
            renderer
            winW
            winH
            clippedTitle
            (platformLabel <> "  " <> speedLabel)
            (if helpVisible then "F1  CLOSE HELP" else "SPACE  RESUME")

    when (fast && not overlayVisible) $
        renderBadge renderer (winW - 194) 20 174 34 panelSecondary accentOrange "FAST FORWARD"

    when (doubleSpeed && not overlayVisible) $
        renderBadge renderer 20 20 168 34 panelSecondary accentBlue "DOUBLE SPEED"

    forM_ toast (renderToast renderer winW winH overlayVisible)

renderPauseOverlay :: SDL.Renderer -> Int -> String -> String -> String -> Bool -> IO ()
renderPauseOverlay renderer winW title platformLabel speedLabel fast = do
    let leftSections =
            [ UiSection
                "ACTIONS"
                accentOrange
                [ "SPACE  RESUME GAME"
                , "F1     OPEN HELP"
                , "F5     SAVE STATE"
                , "F7     LOAD STATE"
                , "F12    SCREENSHOT"
                ]
            , UiSection
                "SESSION"
                accentBlue
                [ "R      HARD RESET"
                , ".      FRAME STEP"
                , "ESC    QUIT"
                ]
            ]
        rightSections =
            [ UiSection
                "SYSTEM"
                accentBlue
                [ "ROM    " <> fitText 18 title
                , "MODE   " <> platformLabel
                , "CLOCK  " <> speedLabel
                , "SPEED  " <> (if fast then "FAST" else "NORMAL")
                ]
            , UiSection
                "CONTROLS"
                accentOrange
                [ "Z/X    A/B"
                , "ENTER  START"
                , "RSHIFT SELECT"
                , "ARROWS DPAD"
                , "TAB    FAST FORWARD"
                ]
            ]
    renderOverlayColumns renderer winW leftSections rightSections

renderHelpOverlay :: SDL.Renderer -> Int -> String -> String -> String -> Bool -> IO ()
renderHelpOverlay renderer winW title platformLabel speedLabel fast = do
    let leftSections =
            [ UiSection
                "EMULATION"
                accentOrange
                [ "F1     CLOSE HELP"
                , "SPACE  PAUSE OR RESUME"
                , ".      FRAME STEP"
                , "TAB    FAST FORWARD"
                , "ESC    QUIT"
                ]
            , UiSection
                "STATE"
                accentBlue
                [ "F5     SAVE STATE"
                , "F7     LOAD STATE"
                , "F12    SCREENSHOT"
                , "R      HARD RESET"
                ]
            ]
        rightSections =
            [ UiSection
                "INPUT"
                accentOrange
                [ "Z      A"
                , "X      B"
                , "ENTER  START"
                , "RSHIFT SELECT"
                , "ARROWS DPAD"
                ]
            , UiSection
                "SYSTEM"
                accentBlue
                [ "ROM    " <> fitText 18 title
                , "MODE   " <> platformLabel
                , "CLOCK  " <> speedLabel
                , "SPEED  " <> (if fast then "FAST" else "NORMAL")
                ]
            ]
    renderOverlayColumns renderer winW leftSections rightSections

renderOverlayColumns :: SDL.Renderer -> Int -> [UiSection] -> [UiSection] -> IO ()
renderOverlayColumns renderer winW leftSections rightSections = do
    let panelY = 92
        panelW = 262
        panelH = 388
        leftX = 38
        rightX = winW - leftX - panelW
    drawPanel renderer panelPrimary accentOrange leftX panelY panelW panelH
    drawPanel renderer panelPrimary accentBlue rightX panelY panelW panelH
    renderSections renderer (leftX + 14) (panelY + 16) (panelW - 28) leftSections
    renderSections renderer (rightX + 14) (panelY + 16) (panelW - 28) rightSections

renderOverlayHeader :: SDL.Renderer -> Int -> String -> IO ()
renderOverlayHeader renderer winW label = do
    let w = 224
        h = 46
        x = (winW - w) `div` 2
        y = 24
    drawPanel renderer panelOverlay accentOrange x y w h
    drawCenteredText renderer 3 textPrimary (winW `div` 2) (y + 12) label

renderStatusBar :: SDL.Renderer -> Int -> Int -> String -> String -> String -> IO ()
renderStatusBar renderer winW winH leftText centerText rightText = do
    let x = 24
        y = winH - 52
        w = winW - 48
        h = 30
    drawPanel renderer panelOverlay accentBlue x y w h
    drawTextShadowed renderer 2 textPrimary (x + 14) (y + 8) leftText
    drawCenteredText renderer 2 textMuted (winW `div` 2) (y + 8) centerText
    drawRightText renderer 2 textSecondary (x + w - 14) (y + 8) rightText

renderBadge :: SDL.Renderer -> Int -> Int -> Int -> Int -> SDL.V4 Word8 -> SDL.V4 Word8 -> String -> IO ()
renderBadge renderer x y w h fill accent label = do
    drawPanel renderer fill accent x y w h
    drawCenteredText renderer 2 textPrimary (x + w `div` 2) (y + 10) label

renderToast :: SDL.Renderer -> Int -> Int -> Bool -> Toast -> IO ()
renderToast renderer winW winH overlayVisible toast = do
    let message = fitText 24 (toastMessage toast)
        (fillColor, accentColor) = case toastStyle toast of
            ToastInfo -> (panelSecondary, accentBlue)
            ToastSuccess -> (successFill, successAccent)
            ToastFailure -> (failureFill, failureAccent)
        w = max 180 (textWidth 2 message + 28)
        h = 34
        x = winW - w - 20
        y
            | overlayVisible = winH - 98
            | otherwise = winH - 54
    drawPanel renderer fillColor accentColor x y w h
    drawTextShadowed renderer 2 textPrimary (x + 14) (y + 10) message

renderSections :: SDL.Renderer -> Int -> Int -> Int -> [UiSection] -> IO ()
renderSections renderer startX startY sectionWidth = go startY
  where
    go _ [] = pure ()
    go y (section : rest) = do
        nextY <- renderSection renderer startX y sectionWidth section
        go nextY rest

renderSection :: SDL.Renderer -> Int -> Int -> Int -> UiSection -> IO Int
renderSection renderer x y sectionWidth section = do
    drawTextShadowed renderer 2 (sectionAccent section) x y (sectionHeader section)
    fillRect renderer (sectionAccent section) x (y + 18) sectionWidth 2
    forM_
        (zip [0 :: Int ..] (sectionRows section))
        ( \(i, row) ->
            drawTextShadowed renderer 2 textPrimary x (y + 28 + i * 20) row
        )
    pure (y + 28 + length (sectionRows section) * 20 + 18)

drawPanel :: SDL.Renderer -> SDL.V4 Word8 -> SDL.V4 Word8 -> Int -> Int -> Int -> Int -> IO ()
drawPanel renderer fillColor accentColor x y w h = do
    fillRect renderer fillColor x y w h
    fillRect renderer accentColor x y w 4
    fillRect renderer textSecondary x y w 1
    fillRect renderer textSecondary x (y + h - 1) w 1
    fillRect renderer textSecondary x y 1 h
    fillRect renderer textSecondary (x + w - 1) y 1 h

fillRect :: SDL.Renderer -> SDL.V4 Word8 -> Int -> Int -> Int -> Int -> IO ()
fillRect renderer color x y w h = do
    SDL.rendererDrawColor renderer $= color
    SDL.fillRect renderer (Just (uiRect x y w h))

uiRect :: Int -> Int -> Int -> Int -> SDL.Rectangle CInt
uiRect x y w h =
    SDL.Rectangle
        (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y)))
        (SDL.V2 (fromIntegral w) (fromIntegral h))

drawCenteredText :: SDL.Renderer -> Int -> SDL.V4 Word8 -> Int -> Int -> String -> IO ()
drawCenteredText renderer scalePx color centerX y text =
    drawTextShadowed renderer scalePx color (centerX - textWidth scalePx text `div` 2) y text

drawRightText :: SDL.Renderer -> Int -> SDL.V4 Word8 -> Int -> Int -> String -> IO ()
drawRightText renderer scalePx color rightX y text =
    drawTextShadowed renderer scalePx color (rightX - textWidth scalePx text) y text

drawTextShadowed :: SDL.Renderer -> Int -> SDL.V4 Word8 -> Int -> Int -> String -> IO ()
drawTextShadowed renderer scalePx color x y text = do
    drawText renderer scalePx shadowColor (x + scalePx) (y + scalePx) text
    drawText renderer scalePx color x y text

drawText :: SDL.Renderer -> Int -> SDL.V4 Word8 -> Int -> Int -> String -> IO ()
drawText renderer scalePx color x y text =
    forM_
        (zip [0 :: Int ..] (map toUpper text))
        (\(i, ch) -> drawGlyph renderer scalePx color (x + i * glyphAdvance scalePx) y ch)

drawGlyph :: SDL.Renderer -> Int -> SDL.V4 Word8 -> Int -> Int -> Char -> IO ()
drawGlyph renderer scalePx color x y ch =
    forM_
        (zip [0 :: Int ..] (glyphRows ch))
        ( \(rowIx, rowBits) ->
            forM_
                [0 .. 4 :: Int]
                ( \colIx ->
                    when (testBit rowBits (4 - colIx)) $
                        fillRect
                            renderer
                            color
                            (x + colIx * scalePx)
                            (y + rowIx * scalePx)
                            scalePx
                            scalePx
                )
        )

glyphAdvance :: Int -> Int
glyphAdvance scalePx = 6 * scalePx

textWidth :: Int -> String -> Int
textWidth scalePx text = length text * glyphAdvance scalePx

fitText :: Int -> String -> String
fitText maxChars text
    | length text <= maxChars = text
    | maxChars <= 3 = take maxChars text
    | otherwise = take (maxChars - 3) text <> "..."

glyphRows :: Char -> [Word8]
glyphRows raw = case toUpper raw of
    ' ' -> [0, 0, 0, 0, 0, 0, 0]
    'A' -> [0x0E, 0x11, 0x11, 0x1F, 0x11, 0x11, 0x11]
    'B' -> [0x1E, 0x11, 0x11, 0x1E, 0x11, 0x11, 0x1E]
    'C' -> [0x0E, 0x11, 0x10, 0x10, 0x10, 0x11, 0x0E]
    'D' -> [0x1E, 0x11, 0x11, 0x11, 0x11, 0x11, 0x1E]
    'E' -> [0x1F, 0x10, 0x10, 0x1E, 0x10, 0x10, 0x1F]
    'F' -> [0x1F, 0x10, 0x10, 0x1E, 0x10, 0x10, 0x10]
    'G' -> [0x0E, 0x11, 0x10, 0x17, 0x11, 0x11, 0x0E]
    'H' -> [0x11, 0x11, 0x11, 0x1F, 0x11, 0x11, 0x11]
    'I' -> [0x1F, 0x04, 0x04, 0x04, 0x04, 0x04, 0x1F]
    'J' -> [0x07, 0x02, 0x02, 0x02, 0x12, 0x12, 0x0C]
    'K' -> [0x11, 0x12, 0x14, 0x18, 0x14, 0x12, 0x11]
    'L' -> [0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1F]
    'M' -> [0x11, 0x1B, 0x15, 0x15, 0x11, 0x11, 0x11]
    'N' -> [0x11, 0x19, 0x15, 0x13, 0x11, 0x11, 0x11]
    'O' -> [0x0E, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0E]
    'P' -> [0x1E, 0x11, 0x11, 0x1E, 0x10, 0x10, 0x10]
    'Q' -> [0x0E, 0x11, 0x11, 0x11, 0x15, 0x12, 0x0D]
    'R' -> [0x1E, 0x11, 0x11, 0x1E, 0x14, 0x12, 0x11]
    'S' -> [0x0F, 0x10, 0x10, 0x0E, 0x01, 0x01, 0x1E]
    'T' -> [0x1F, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04]
    'U' -> [0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0E]
    'V' -> [0x11, 0x11, 0x11, 0x11, 0x11, 0x0A, 0x04]
    'W' -> [0x11, 0x11, 0x11, 0x15, 0x15, 0x1B, 0x11]
    'X' -> [0x11, 0x11, 0x0A, 0x04, 0x0A, 0x11, 0x11]
    'Y' -> [0x11, 0x11, 0x0A, 0x04, 0x04, 0x04, 0x04]
    'Z' -> [0x1F, 0x01, 0x02, 0x04, 0x08, 0x10, 0x1F]
    '0' -> [0x0E, 0x11, 0x13, 0x15, 0x19, 0x11, 0x0E]
    '1' -> [0x04, 0x0C, 0x04, 0x04, 0x04, 0x04, 0x0E]
    '2' -> [0x0E, 0x11, 0x01, 0x02, 0x04, 0x08, 0x1F]
    '3' -> [0x1E, 0x01, 0x01, 0x0E, 0x01, 0x01, 0x1E]
    '4' -> [0x02, 0x06, 0x0A, 0x12, 0x1F, 0x02, 0x02]
    '5' -> [0x1F, 0x10, 0x10, 0x1E, 0x01, 0x01, 0x1E]
    '6' -> [0x0E, 0x10, 0x10, 0x1E, 0x11, 0x11, 0x0E]
    '7' -> [0x1F, 0x01, 0x02, 0x04, 0x08, 0x10, 0x10]
    '8' -> [0x0E, 0x11, 0x11, 0x0E, 0x11, 0x11, 0x0E]
    '9' -> [0x0E, 0x11, 0x11, 0x0F, 0x01, 0x02, 0x1C]
    '.' -> [0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C]
    ',' -> [0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x08]
    ':' -> [0x00, 0x0C, 0x0C, 0x00, 0x0C, 0x0C, 0x00]
    '!' -> [0x04, 0x04, 0x04, 0x04, 0x04, 0x00, 0x04]
    '?' -> [0x0E, 0x11, 0x01, 0x02, 0x04, 0x00, 0x04]
    '-' -> [0x00, 0x00, 0x00, 0x1F, 0x00, 0x00, 0x00]
    '/' -> [0x01, 0x02, 0x04, 0x08, 0x10, 0x00, 0x00]
    '\'' -> [0x04, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00]
    '&' -> [0x0C, 0x12, 0x14, 0x08, 0x15, 0x12, 0x0D]
    '(' -> [0x02, 0x04, 0x08, 0x08, 0x08, 0x04, 0x02]
    ')' -> [0x08, 0x04, 0x02, 0x02, 0x02, 0x04, 0x08]
    _ -> [0x0E, 0x11, 0x01, 0x02, 0x04, 0x00, 0x04]

handlePending ::
    FilePath ->
    Hotkeys ->
    UiState ->
    IORef Machine ->
    Cartridge ->
    Maybe BS.ByteString ->
    IO ()
handlePending romPath hk ui machineRef cart bootRom = do
    machine <- readIORef machineRef
    saveReq <- readIORef (hkSaveReq hk)
    when saveReq $ do
        writeIORef (hkSaveReq hk) False
        blob <- Snap.save machine
        let path = romPath <> ".state"
        r <- try (BS.writeFile path blob) :: IO (Either IOException ())
        case r of
            Right () -> do
                putStrLn ("state:    saved " <> path <> " (" <> show (BS.length blob) <> " B)")
                pushToast ui ToastSuccess "State saved"
            Left e -> do
                putStrLn ("state:    save failed: " <> show e)
                pushToast ui ToastFailure "Save failed"
    loadReq <- readIORef (hkLoadReq hk)
    when loadReq $ do
        writeIORef (hkLoadReq hk) False
        let path = romPath <> ".state"
        r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
        case r of
            Right blob -> do
                res <- Snap.load blob machine
                case res of
                    Right () -> do
                        putStrLn ("state:    loaded " <> path)
                        pushToast ui ToastSuccess "State loaded"
                    Left err -> do
                        putStrLn ("state:    load failed: " <> show err)
                        pushToast ui ToastFailure "Load failed"
            Left _ -> do
                putStrLn ("state:    no " <> path <> " to load")
                pushToast ui ToastFailure "No state file"
    shotReq <- readIORef (hkShotReq hk)
    when shotReq $ do
        writeIORef (hkShotReq hk) False
        fb <- Ppu.framebufferRgb (Bus.busPpu (machineBus machine))
        ts <- floor <$> getPOSIXTime :: IO Int
        let path = romPath <> "-" <> show ts <> ".ppm"
        r <- try (writePpm path fb) :: IO (Either IOException ())
        case r of
            Right () -> do
                putStrLn ("shot:     wrote " <> path)
                pushToast ui ToastSuccess "Screenshot written"
            Left e -> do
                putStrLn ("shot:     failed: " <> show e)
                pushToast ui ToastFailure "Screenshot failed"
    resetReq <- readIORef (hkResetReq hk)
    when resetReq $ do
        writeIORef (hkResetReq hk) False
        machine' <- machineFromCartridgeWithBoot bootRom cart
        writeIORef machineRef machine'
        putStrLn "reset:    machine rebuilt from cartridge"
        pushToast ui ToastInfo "Machine reset"

writePpm :: FilePath -> V.Vector Word8 -> IO ()
writePpm path fb = do
    let header =
            BS.pack
                ( map (fromIntegral . fromEnum) $
                    "P6\n" <> show gbWidth <> " " <> show gbHeight <> "\n255\n"
                )
        body = BS.pack (V.toList fb)
    BS.writeFile path (header <> body)

handleEvent :: Hotkeys -> UiState -> JoypadState -> SDL.Event -> IO ()
handleEvent hk ui jp ev = case SDL.eventPayload ev of
    SDL.QuitEvent -> writeIORef (hkQuit hk) True
    SDL.ControllerButtonEvent cev ->
        let pressed = SDL.controllerButtonEventState cev == SDLGC.ControllerButtonPressed
         in case mapPad (SDL.controllerButtonEventButton cev) of
                Just btn -> Joypad.setButton btn pressed jp
                Nothing -> pure ()
    SDL.ControllerAxisEvent aev ->
        let val = SDL.controllerAxisEventValue aev
            pos = val > axisDeadzone
            neg = val < (-axisDeadzone)
         in case SDL.controllerAxisEventAxis aev of
                SDLGC.ControllerAxisLeftX -> do
                    Joypad.setButton ButtonLeft neg jp
                    Joypad.setButton ButtonRight pos jp
                SDLGC.ControllerAxisLeftY -> do
                    Joypad.setButton ButtonUp neg jp
                    Joypad.setButton ButtonDown pos jp
                _ -> pure ()
    SDL.ControllerDeviceEvent cev ->
        when (SDL.controllerDeviceEventConnection cev == SDLGC.ControllerDeviceAdded) $ do
            devs <- SDLGC.availableControllers
            mapM_ SDLGC.openController devs
    SDL.KeyboardEvent kev -> do
        let pressed = SDL.keyboardEventKeyMotion kev == SDL.Pressed
            scancode = SDL.keysymScancode (SDL.keyboardEventKeysym kev)
            isRepeat = SDL.keyboardEventRepeat kev
        case scancode of
            SDL.ScancodeEscape ->
                when pressed $ do
                    helpVisible <- readIORef (uiHelpVisible ui)
                    if helpVisible
                        then writeIORef (uiHelpVisible ui) False
                        else writeIORef (hkQuit hk) True
            SDL.ScancodeSpace ->
                when (pressed && not isRepeat) $ do
                    helpVisible <- readIORef (uiHelpVisible ui)
                    unless helpVisible $ do
                        modifyIORef' (hkPaused hk) not
                        paused <- readIORef (hkPaused hk)
                        pushToast ui ToastInfo (if paused then "Paused" else "Resumed")
            SDL.ScancodeF1 ->
                when (pressed && not isRepeat) $ do
                    helpVisible <- readIORef (uiHelpVisible ui)
                    writeIORef (uiHelpVisible ui) (not helpVisible)
            SDL.ScancodeTab -> writeIORef (hkFastFwd hk) pressed
            SDL.ScancodeF5 ->
                when (pressed && not isRepeat) (writeIORef (hkSaveReq hk) True)
            SDL.ScancodeF7 ->
                when (pressed && not isRepeat) (writeIORef (hkLoadReq hk) True)
            SDL.ScancodeF12 ->
                when (pressed && not isRepeat) (writeIORef (hkShotReq hk) True)
            SDL.ScancodePeriod ->
                when (pressed && not isRepeat) $ do
                    helpVisible <- readIORef (uiHelpVisible ui)
                    unless helpVisible (writeIORef (hkFrameStepReq hk) True)
            SDL.ScancodeR ->
                when (pressed && not isRepeat) (writeIORef (hkResetReq hk) True)
            _ -> case mapKey scancode of
                Just btn -> Joypad.setButton btn pressed jp
                Nothing -> pure ()
    _ -> pure ()

mapKey :: SDL.Scancode -> Maybe Button
mapKey s = case s of
    SDL.ScancodeZ -> Just ButtonA
    SDL.ScancodeX -> Just ButtonB
    SDL.ScancodeReturn -> Just ButtonStart
    SDL.ScancodeRShift -> Just ButtonSelect
    SDL.ScancodeUp -> Just ButtonUp
    SDL.ScancodeDown -> Just ButtonDown
    SDL.ScancodeLeft -> Just ButtonLeft
    SDL.ScancodeRight -> Just ButtonRight
    _ -> Nothing

{- | Map an SDL game-controller button to a GB joypad button. The controller's "A" face button maps
to GB A (south = primary action), "B" maps to GB B, the menu pair to Start\/Select, and the D-pad to
the equivalent direction.
-}
mapPad :: SDLGC.ControllerButton -> Maybe Button
mapPad b = case b of
    SDLGC.ControllerButtonA -> Just ButtonA
    SDLGC.ControllerButtonB -> Just ButtonB
    SDLGC.ControllerButtonStart -> Just ButtonStart
    SDLGC.ControllerButtonBack -> Just ButtonSelect
    SDLGC.ControllerButtonDpadUp -> Just ButtonUp
    SDLGC.ControllerButtonDpadDown -> Just ButtonDown
    SDLGC.ControllerButtonDpadLeft -> Just ButtonLeft
    SDLGC.ControllerButtonDpadRight -> Just ButtonRight
    _ -> Nothing

{- | Streaming-update path: 'fb' is already in RGB888 with one byte per channel, so the SDL upload
is a single 'BS.pack' away.
-}
updateTextureRgb :: SDL.Texture -> V.Vector Word8 -> IO ()
updateTextureRgb tex fb = do
    let bs = BS.pack (V.toList fb)
    _ <- SDL.updateTexture tex Nothing bs (fromIntegral (gbWidth * 3))
    pure ()

{- | Diagnostic: open the audio device and play a 440 Hz sine tone for two seconds, bypassing the APU.
If you hear nothing here, the SDL audio path is the problem; if you hear the tone, the APU is the problem.
-}
audioTest :: IO ()
audioTest = do
    SDL.initialize [SDL.InitAudio]
    putStrLn "audio test: opening device for 440 Hz sine, 2 s..."
    let totalSamples = Apu.sampleRate * 2 -- 2 seconds, mono samples per side
        toneFreq = 440 :: Double
        rate = fromIntegral Apu.sampleRate :: Double
        sineAt :: Int -> Int16
        sineAt i =
            let t = fromIntegral i / rate :: Double
                v = sin (2 * pi * toneFreq * t) * 8000
             in fromIntegral (round v :: Int)
        -- Interleave L and R.
        samples =
            concatMap
                (\i -> let s = sineAt i in [s, s])
                [0 .. totalSamples - 1]
    audioBuf <- newMVar samples
    let spec =
            SDL.OpenDeviceSpec
                { SDL.openDeviceFreq = SDL.Desire (fromIntegral Apu.sampleRate)
                , SDL.openDeviceFormat = SDL.Desire SDL.Signed16BitNativeAudio
                , SDL.openDeviceChannels = SDL.Desire SDL.Stereo
                , SDL.openDeviceSamples = 1024
                , SDL.openDeviceCallback = audioCallback audioBuf
                , SDL.openDeviceUsage = SDL.ForPlayback
                , SDL.openDeviceName = Nothing
                }
    (dev, _actual) <- SDL.openAudioDevice spec
    putStrLn "device opened."
    SDL.setAudioDevicePlaybackState dev SDL.Play
    threadDelay 2000000
    SDL.setAudioDevicePlaybackState dev SDL.Pause
    SDL.closeAudioDevice dev
    SDL.quit
    putStrLn "audio test done."
