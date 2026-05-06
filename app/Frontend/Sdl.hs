{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
    startupScreen,
    audioTest,
) where

import Codec.Picture (Image, PaletteCreationMethod (..), PaletteOptions (..), PixelRGB8 (..), generateImage, palettize)
import Codec.Picture.Gif (GifDelay, GifLooping (..), writeGifImages)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Char (isSpace, toUpper)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64, Word8)
import Development.GitRev (gitBranch, gitHash)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
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
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeDirectory, (</>))
#ifdef mingw32_HOST_OS
import System.Process (readProcess)
#elif defined(darwin_HOST_OS)
import System.Process (readProcess)
#else
import System.Process (readProcessWithExitCode)
#endif

gbWidth, gbHeight :: Int
gbWidth = 160
gbHeight = 144

windowSize :: Int -> (Int, Int)
windowSize s = (gbWidth * s, gbHeight * s)

frameNs :: Word64
frameNs = 16742706

toastDurationNs :: Word64
toastDurationNs = frameNs * 240

doubleSpeedBadgeDurationNs :: Word64
doubleSpeedBadgeDurationNs = frameNs * 180

-- | Analog stick deadzone (~25 % of the 32 767 maximum).
axisDeadzone :: Int16
axisDeadzone = 8000

-- | Cap the audio buffer so an emulator running ahead of the audio device doesn't accumulate unbounded samples.
maxBufferedSamples :: Int
maxBufferedSamples = 9600 -- ~100 ms of stereo samples at 48 kHz

rgbFramebufferBytes :: Int
rgbFramebufferBytes = gbWidth * gbHeight * 3

data RgbFrameStage = RgbFrameStage
    { rgbFramePtr :: !(ForeignPtr Word8)
    , rgbFrameBytes :: !BS.ByteString
    }

newRgbFrameStage :: IO RgbFrameStage
newRgbFrameStage = do
    fp <- BSI.mallocByteString rgbFramebufferBytes
    pure
        ( RgbFrameStage
            { rgbFramePtr = fp
            , rgbFrameBytes = BSI.fromForeignPtr fp 0 rgbFramebufferBytes
            }
        )

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
    , hkFullscreenReq :: !(IORef Bool)
    -- ^ F11: toggle fullscreen/windowed.
    , hkGifToggle :: !(IORef Bool)
    -- ^ Shift+F12: start or stop GIF recording.
    , hkOpenReq :: !(IORef Bool)
    -- ^ "O": quit the current session and request a new ROM to be loaded.
    }

data PaceMode
    = PaceVSync
    | PaceSleep
    deriving (Eq, Show)

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
    , toastHideAfterNs :: !Word64
    }

data UiState = UiState
    { uiHelpVisible :: !(IORef Bool)
    , uiPerfVisible :: !(IORef Bool)
    , uiFrameCounter :: !(IORef Word64)
    -- ^ Count of presented frames. Used for the perf overlay history and GIF capture cadence.
    , uiToasts :: !(IORef [Toast])
    , uiCurrentSlot :: !(IORef Int)
    , uiFrameTimes :: !(IORef [Word64])
    -- ^ Ring of up to 61 frame-present timestamps (ns). Used to compute FPS/frame-time.
    , uiDoubleSpeedBadgeUntil :: !(IORef Word64)
    -- ^ Monotonic timestamp in ns after which the "DOUBLE SPEED" badge is hidden. 0 = not active.
    , uiGifFrames :: !(IORef (Maybe [V.Vector Word8]))
    -- ^ Nothing = not recording; Just frames = recording (newest frame first).
    }

newUiState :: IO UiState
newUiState =
    UiState
        <$> newIORef False
        <*> newIORef False
        <*> newIORef 0
        <*> newIORef []
        <*> newIORef 1
        <*> newIORef []
        <*> newIORef 0
        <*> newIORef Nothing

recordPresentedFrame :: UiState -> IO Word64
recordPresentedFrame ui = do
    frame0 <- readIORef (uiFrameCounter ui)
    let frame = frame0 + 1
    writeIORef (uiFrameCounter ui) frame
    pure frame

pruneUiDeadlines :: Word64 -> UiState -> IO ()
pruneUiDeadlines nowNs ui =
    modifyIORef' (uiToasts ui) (filter (\toast -> toastHideAfterNs toast > nowNs))

pushToast :: UiState -> ToastStyle -> String -> IO ()
pushToast ui style message = do
    nowNs <- getMonotonicTimeNSec
    let toast =
            Toast
                { toastStyle = style
                , toastMessage = message
                , toastHideAfterNs = nowNs + toastDurationNs
                }
    modifyIORef'
        (uiToasts ui)
        ( \toasts ->
            let active = filter (\entry -> toastHideAfterNs entry > nowNs) toasts
             in if length active < 3
                    then active ++ [toast]
                    else take 2 active ++ [toast]
        )

currentToast :: UiState -> IO (Maybe Toast)
currentToast ui = listToMaybe <$> readIORef (uiToasts ui)

nextUiDeadline :: Word64 -> UiState -> IO (Maybe Word64)
nextUiDeadline nowNs ui = do
    toasts <- readIORef (uiToasts ui)
    badgeUntil <- readIORef (uiDoubleSpeedBadgeUntil ui)
    let toastDeadlines = [toastHideAfterNs toast | toast <- toasts, toastHideAfterNs toast > nowNs]
        badgeDeadlines = [badgeUntil | badgeUntil > nowNs]
    pure $
        case toastDeadlines ++ badgeDeadlines of
            [] -> Nothing
            deadlines -> Just (minimum deadlines)

fallbackTitle :: FilePath -> String
fallbackTitle path =
    let fileName = reverse (takeWhile (\c -> c /= '/' && c /= '\\') (reverse path))
        stem = reverse (drop 1 (dropWhile (/= '.') (reverse fileName)))
     in if null stem then fileName else stem

-- | Folder that holds all save data for a ROM: <romdir>/<romstem>/
romDir :: FilePath -> FilePath
romDir romPath = takeDirectory romPath </> takeBaseName romPath

slotPath :: FilePath -> Int -> FilePath
slotPath romPath slot = romDir romPath </> ("slot" <> show slot <> ".state")

play :: FilePath -> Cartridge -> Maybe BS.ByteString -> Text -> Int -> IO Bool
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
    let buildTag = T.pack $(gitBranch) <> "@" <> T.pack (take 7 $(gitHash))
    window <-
        SDL.createWindow
            ("Ocelot Emulator (" <> buildTag <> ")")
            SDL.defaultWindow
                { SDL.windowInitialSize =
                    SDL.V2 (fromIntegral winW) (fromIntegral winH)
                , SDL.windowResizable = True
                }
    (renderer, paceMode) <- createRendererWithPacing window
    SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
    texture <-
        SDL.createTexture
            renderer
            SDL.RGB24
            SDL.TextureAccessStreaming
            (SDL.V2 (fromIntegral gbWidth) (fromIntegral gbHeight))
    rgbStage <- newRgbFrameStage

    audioBuf <- newMVar Seq.empty
    audioDev <- openAudio audioBuf
    audioPlaying <- newIORef True

    machine0 <- machineFromCartridgeWithBoot bootRom cart
    machineRef <- newIORef machine0
    hk <- newHotkeys
    ui <- newUiState

    SDL.setAudioDevicePlaybackState audioDev SDL.Play

    loop romPath titleStr hk ui machineRef cart bootRom renderer texture rgbStage paceMode audioDev audioPlaying audioBuf window

    SDL.setAudioDevicePlaybackState audioDev SDL.Pause
    SDL.closeAudioDevice audioDev
    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    readIORef (hkOpenReq hk)

openAudio :: MVar (Seq.Seq Int16) -> IO SDL.AudioDevice
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
    MVar (Seq.Seq Int16) ->
    SDL.AudioFormat sampleType ->
    VSM.IOVector sampleType ->
    IO ()
audioCallback buf fmt out = case fmt of
    SDL.Signed16BitNativeAudio -> writeInt16 buf out
    SDL.Signed16BitLEAudio -> writeInt16 buf out
    SDL.Signed16BitBEAudio -> writeInt16 buf out
    _ -> pure ()

writeInt16 :: MVar (Seq.Seq Int16) -> VSM.IOVector Int16 -> IO ()
writeInt16 buf out = do
    let !needed = VSM.length out
    samples <-
        modifyMVar
            buf
            ( \xs -> do
                let (taken, rest) = Seq.splitAt needed xs
                pure (rest, taken)
            )
    let go !i queued
            | i >= needed = pure ()
            | otherwise =
                case Seq.viewl queued of
                    Seq.EmptyL -> do
                        VSM.write out i 0
                        go (i + 1) queued
                    sample Seq.:< rest -> do
                        VSM.write out i sample
                        go (i + 1) rest
    go 0 samples

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
    RgbFrameStage ->
    PaceMode ->
    SDL.AudioDevice ->
    IORef Bool ->
    MVar (Seq.Seq Int16) ->
    SDL.Window ->
    IO ()
loop romPath titleStr hk ui machineRef cart bootRom renderer texture rgbStage paceMode audioDev audioPlaying audioBuf window = do
    quit <- readIORef (hkQuit hk)
    unless quit $ do
        machine <- readIORef machineRef
        paused0 <- readIORef (hkPaused hk)
        helpVisible0 <- readIORef (uiHelpVisible ui)
        now0 <- getMonotonicTimeNSec
        pruneUiDeadlines now0 ui
        deadline <- nextUiDeadline now0 ui
        events <- gatherEvents (paused0 || helpVisible0) now0 deadline
        mapM_ (handleEvent hk ui (Bus.busJoypad (machineBus machine))) events

        -- One-shot hotkeys: handle save/load/screenshot/reset requests.
        handlePending romPath hk ui machineRef cart bootRom

        -- Toggle fullscreen on request.
        fsReq <- readIORef (hkFullscreenReq hk)
        when fsReq $ do
            writeIORef (hkFullscreenReq hk) False
            cfg <- SDL.getWindowConfig window
            SDL.setWindowMode window $ case SDL.windowMode cfg of
                SDL.FullscreenDesktop -> SDL.Windowed
                _ -> SDL.FullscreenDesktop

        -- Re-read in case reset swapped the machine.
        machine' <- readIORef machineRef

        -- Show the "DOUBLE SPEED" badge for 180 frames when double-speed activates;
        -- reset the timer when double-speed turns off so it fires again next time.
        doubleSpeedNow <- readIORef (Bus.busDoubleSpeed (machineBus machine'))
        dsBadge <- readIORef (uiDoubleSpeedBadgeUntil ui)
        if doubleSpeedNow
            then when (dsBadge == 0) $ do
                nowBadge <- getMonotonicTimeNSec
                writeIORef (uiDoubleSpeedBadgeUntil ui) (nowBadge + doubleSpeedBadgeDurationNs)
            else when (dsBadge /= 0) $ writeIORef (uiDoubleSpeedBadgeUntil ui) 0
        paused <- readIORef (hkPaused hk)
        helpVisible <- readIORef (uiHelpVisible ui)
        fast <- readIORef (hkFastFwd hk)
        stepOnce <- readIORef (hkFrameStepReq hk)
        when stepOnce (writeIORef (hkFrameStepReq hk) False)
        let frames = if fast then 4 else 1 :: Int
            shouldRun = (not paused && not helpVisible) || (stepOnce && paused && not helpVisible)
            audioShouldPlay = not paused && not helpVisible

        syncAudioPlayback audioDev audioPlaying audioBuf audioShouldPlay

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
            unless (null samples) $ appendAudioSamples audioBuf samples

        nowUi <- getMonotonicTimeNSec
        pruneUiDeadlines nowUi ui

        -- Query live window size so resize and fullscreen are handled correctly.
        SDL.V2 wW wH <- SDL.get (SDL.windowSize window)
        let winW = fromIntegral wW :: Int
            winH = fromIntegral wH :: Int
            bestScale = max 1 (min (winW `div` gbWidth) (winH `div` gbHeight))
            dstW = gbWidth * bestScale
            dstH = gbHeight * bestScale
            dstX = (winW - dstW) `div` 2
            dstY = (winH - dstH) `div` 2
            dst =
                SDL.Rectangle
                    (SDL.P (SDL.V2 (fromIntegral dstX) (fromIntegral dstY)))
                    (SDL.V2 (fromIntegral dstW) (fromIntegral dstH))

        withForeignPtr (rgbFramePtr rgbStage) $ \ptr ->
            Ppu.copyFramebufferRgb ptr (Bus.busPpu (machineBus machine'))
        updateTextureRgb texture (rgbFrameBytes rgbStage)
        SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
        SDL.clear renderer
        SDL.copy renderer texture Nothing (Just dst)
        renderUi renderer ui titleStr machine' paused fast paceMode nowUi winW winH
        SDL.present renderer
        frame <- recordPresentedFrame ui

        -- Capture every other frame to halve GIF file size (~30 fps effective).
        gifRec <- readIORef (uiGifFrames ui)
        case gifRec of
            Just fs | even frame -> do
                fbRgbFrame <- Ppu.framebufferRgb (Bus.busPpu (machineBus machine'))
                writeIORef (uiGifFrames ui) (Just (fbRgbFrame : fs))
            _ -> pure ()

        unless fast (paceFrame paceMode frameStartNs)

        when shouldRun $ do
            -- Record after pacing so consecutive timestamps span the full frame
            -- (computation + sleep), giving an accurate FPS/frame-time reading.
            frameEndNs <- getMonotonicTimeNSec
            modifyIORef' (uiFrameTimes ui) $ \ts ->
                let ts' = ts ++ [frameEndNs]
                 in if length ts' > 61 then drop 1 ts' else ts'
        loop romPath titleStr hk ui machineRef cart bootRom renderer texture rgbStage paceMode audioDev audioPlaying audioBuf window

gatherEvents :: Bool -> Word64 -> Maybe Word64 -> IO [SDL.Event]
gatherEvents waitMode nowNs mDeadline
    | not waitMode = SDL.pollEvents
    | otherwise = do
        first <- case mDeadline of
            Just deadline ->
                SDL.waitEventTimeout (fromIntegral (max 0 ((deadline - nowNs + 999999) `div` 1000000)))
            Nothing -> Just <$> SDL.waitEvent
        rest <- SDL.pollEvents
        pure (maybe rest (: rest) first)

appendAudioSamples :: MVar (Seq.Seq Int16) -> [Int16] -> IO ()
appendAudioSamples audioBuf samples =
    modifyMVar_
        audioBuf
        ( \existing -> do
            let !combined = existing Seq.>< Seq.fromList samples
                !overflow = Seq.length combined - maxBufferedSamples
            pure $
                if overflow > 0
                    then Seq.drop overflow combined
                    else combined
        )

syncAudioPlayback :: SDL.AudioDevice -> IORef Bool -> MVar (Seq.Seq Int16) -> Bool -> IO ()
syncAudioPlayback audioDev audioPlaying audioBuf shouldPlay = do
    playing <- readIORef audioPlaying
    when (playing /= shouldPlay) $ do
        if shouldPlay
            then SDL.setAudioDevicePlaybackState audioDev SDL.Play
            else do
                modifyMVar_ audioBuf (const (pure Seq.empty))
                SDL.setAudioDevicePlaybackState audioDev SDL.Pause
        writeIORef audioPlaying shouldPlay

paceFrame :: PaceMode -> Word64 -> IO ()
paceFrame PaceVSync _ = pure ()
paceFrame PaceSleep frameStartNs = do
    now <- getMonotonicTimeNSec
    let elapsedNs = now - frameStartNs
    when (elapsedNs < frameNs) $
        threadDelay
            ( fromIntegral $
                (frameNs - elapsedNs + 999) `div` 1000
            )

createRendererWithPacing :: SDL.Window -> IO (SDL.Renderer, PaceMode)
createRendererWithPacing window = do
    let vsyncConfig =
            SDL.defaultRenderer
                { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                }
        fallbackConfig =
            SDL.defaultRenderer
                { SDL.rendererType = SDL.AcceleratedRenderer
                }
    vsyncAttempt <- try (SDL.createRenderer window (-1) vsyncConfig) :: IO (Either SDL.SDLException SDL.Renderer)
    case vsyncAttempt of
        Right renderer -> pure (renderer, PaceVSync)
        Left _ -> do
            renderer <- SDL.createRenderer window (-1) fallbackConfig
            pure (renderer, PaceSleep)

panelPrimary, panelSecondary, panelOverlay, accentOrange, accentBlue :: SDL.V4 Word8
-- DMG-inspired backgrounds: near-black forest green, like the DMG screen surround.
panelPrimary = SDL.V4 0x0B 0x16 0x0B 0xEC
panelSecondary = SDL.V4 0x0E 0x1C 0x0E 0xF0
panelOverlay = SDL.V4 0x0C 0x18 0x0C 0xDC
-- DMG LCD brightest shade — the soft yellowish-green of the original screen.
accentOrange = SDL.V4 0x9B 0xBC 0x0F 0xFF
-- Slightly muted teal-green for secondary accents.
accentBlue = SDL.V4 0x5A 0xA8 0x78 0xFF

-- Platform accent colors: DMG reuses the main accent; CGB = atomic purple.
dmgAccent, cgbAccent :: SDL.V4 Word8
dmgAccent = accentOrange
cgbAccent = SDL.V4 0xAA 0x55 0xDD 0xFF

textPrimary, textSecondary, textMuted, shadowColor :: SDL.V4 Word8
-- Slight green tint on text so it reads as "viewed through the LCD".
textPrimary = SDL.V4 0xD8 0xE8 0xC8 0xFF
textSecondary = SDL.V4 0x88 0x98 0x80 0xFF
textMuted = SDL.V4 0xC0 0xD4 0xB0 0xFF
shadowColor = SDL.V4 0x00 0x00 0x00 0xA8

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

renderUi :: SDL.Renderer -> UiState -> String -> Machine -> Bool -> Bool -> PaceMode -> Word64 -> Int -> Int -> IO ()
renderUi renderer ui title machine paused fast paceMode nowNs winW winH = do
    helpVisible <- readIORef (uiHelpVisible ui)
    perfVisible <- readIORef (uiPerfVisible ui)
    slot <- readIORef (uiCurrentSlot ui)
    frameTimes <- readIORef (uiFrameTimes ui)
    dsBadgeUntil <- readIORef (uiDoubleSpeedBadgeUntil ui)
    toast <- currentToast ui
    let bus = machineBus machine
        isCgb = Bus.busCgb bus
        platformLabel = if isCgb then "CGB" else "DMG"
    doubleSpeed <- readIORef (Bus.busDoubleSpeed bus)
    let speedLabel = if doubleSpeed then "DOUBLE SPEED" else "NORMAL SPEED"
        clippedTitle = fitText 28 title
        overlayVisible = paused || helpVisible

    when overlayVisible $ do
        fillRect renderer (SDL.V4 0x00 0x00 0x00 0x98) 0 0 winW winH
        renderOverlayHeader renderer winW isCgb (if helpVisible then "HELP" else "PAUSED")
        if helpVisible
            then renderHelpOverlay renderer winW clippedTitle platformLabel speedLabel fast slot isCgb
            else renderPauseOverlay renderer winW clippedTitle platformLabel speedLabel fast slot isCgb
        renderStatusBar
            renderer
            winW
            winH
            clippedTitle
            (platformLabel <> "  " <> speedLabel)
            (if helpVisible then "F1  CLOSE HELP" else "SPACE  RESUME")

    gifRecording <- readIORef (uiGifFrames ui)

    when (fast && not overlayVisible) $
        renderBadge renderer (winW - 194) 20 174 34 panelSecondary accentOrange "FAST FORWARD"

    when (isJust gifRecording && not overlayVisible) $
        renderBadge renderer 20 20 68 34 panelSecondary (SDL.V4 0xDD 0x33 0x33 0xFF) "REC"

    let dsBadgeY = if isJust gifRecording then 62 else 20
    when (dsBadgeUntil > nowNs && not overlayVisible) $
        renderBadge renderer 20 dsBadgeY 168 34 panelSecondary accentBlue "DOUBLE SPEED"

    when (perfVisible && not overlayVisible) $
        renderPerfOverlay renderer winW paceMode frameTimes

    forM_ toast (renderToast renderer winW winH overlayVisible)

renderPauseOverlay :: SDL.Renderer -> Int -> String -> String -> String -> Bool -> Int -> Bool -> IO ()
renderPauseOverlay renderer winW title platformLabel speedLabel fast slot isCgb = do
    let leftSections =
            [ UiSection
                "ACTIONS"
                accentOrange
                [ "SPACE  RESUME GAME"
                , "F1     OPEN HELP"
                , "F5     SAVE STATE"
                , "F7     LOAD STATE"
                , "F6     SLOT SELECT"
                , "F12    SCREENSHOT"
                , "SF12   RECORD GIF"
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
                [ "ROM    " <> fitText 12 title
                , "MODE   " <> platformLabel
                , "CLOCK  " <> speedLabel
                , "SPEED  " <> (if fast then "FAST" else "NORMAL")
                , "SLOT   " <> show slot
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
    renderOverlayColumns renderer winW isCgb leftSections rightSections

renderHelpOverlay :: SDL.Renderer -> Int -> String -> String -> String -> Bool -> Int -> Bool -> IO ()
renderHelpOverlay renderer winW title platformLabel speedLabel fast slot isCgb = do
    let leftSections =
            [ UiSection
                "EMULATION"
                accentOrange
                [ "F1     CLOSE HELP"
                , "SPACE  PAUSE/RESUME"
                , ".      FRAME STEP"
                , "TAB    FAST FORWARD"
                , "F11    FULLSCREEN"
                , "P      PERF OVERLAY"
                , "ESC    QUIT"
                ]
            , UiSection
                "STATE"
                accentBlue
                [ "F5     SAVE STATE"
                , "F7     LOAD STATE"
                , "F6     SLOT SELECT"
                , "F12    SCREENSHOT"
                , "SF12   RECORD GIF"
                , "R      HARD RESET"
                , "O      OPEN ROM"
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
                [ "ROM    " <> fitText 12 title
                , "MODE   " <> platformLabel
                , "CLOCK  " <> speedLabel
                , "SPEED  " <> (if fast then "FAST" else "NORMAL")
                , "SLOT   " <> show slot
                ]
            ]
    renderOverlayColumns renderer winW isCgb leftSections rightSections

showFixed1 :: Double -> String
showFixed1 x =
    let i = floor x :: Int
        d = floor (x * 10) `mod` 10 :: Int
     in show i <> "." <> show d

renderPerfOverlay :: SDL.Renderer -> Int -> PaceMode -> [Word64] -> IO ()
renderPerfOverlay renderer winW paceMode frameTimes = do
    let fps = case frameTimes of
            (_ : _ : _) ->
                let spanNs = fromIntegral (last frameTimes - head frameTimes) :: Double
                    n = fromIntegral (length frameTimes - 1) :: Double
                 in n / (spanNs / 1e9)
            _ -> 0
        fpsStr = "FPS  " <> showFixed1 fps
        paceStr = "PACE  " <> paceModeLabel paceMode
        w = max (textWidth 2 fpsStr) (textWidth 2 paceStr) + 28
        h = 54
        x = winW - w - 20
        y = 64
    drawPanel renderer panelSecondary accentBlue x y w h
    drawTextShadowed renderer 2 textPrimary (x + 14) (y + 9) fpsStr
    drawTextShadowed renderer 2 textMuted (x + 14) (y + 29) paceStr

paceModeLabel :: PaceMode -> String
paceModeLabel PaceVSync = "VSYNC"
paceModeLabel PaceSleep = "SLEEP FALLBACK"

sectionH :: UiSection -> Int
sectionH s = length (sectionRows s) * 20 + 46

renderOverlayColumns :: SDL.Renderer -> Int -> Bool -> [UiSection] -> [UiSection] -> IO ()
renderOverlayColumns renderer winW isCgb leftSections rightSections = do
    let panelY = 92
        panelW = 262
        contentH = max (sum (map sectionH leftSections)) (sum (map sectionH rightSections))
        panelH = contentH + 32
        leftX = 38
        rightX = winW - leftX - panelW
        modeAccent = if isCgb then cgbAccent else dmgAccent
    drawPanel renderer panelPrimary modeAccent leftX panelY panelW panelH
    drawPanel renderer panelPrimary accentBlue rightX panelY panelW panelH
    renderSections renderer (leftX + 14) (panelY + 16) (panelW - 28) leftSections
    renderSections renderer (rightX + 14) (panelY + 16) (panelW - 28) rightSections

renderOverlayHeader :: SDL.Renderer -> Int -> Bool -> String -> IO ()
renderOverlayHeader renderer winW isCgb label = do
    let w = 224
        h = 46
        x = (winW - w) `div` 2
        y = 24
        accent = if isCgb then cgbAccent else dmgAccent
    drawPanel renderer panelOverlay accent x y w h
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
    '[' -> [0x1C, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1C]
    ']' -> [0x07, 0x01, 0x01, 0x01, 0x01, 0x01, 0x07]
    '\'' -> [0x04, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00]
    '&' -> [0x0C, 0x12, 0x14, 0x08, 0x15, 0x12, 0x0D]
    '(' -> [0x02, 0x04, 0x08, 0x08, 0x08, 0x04, 0x02]
    ')' -> [0x08, 0x04, 0x02, 0x02, 0x02, 0x04, 0x08]
    '>' -> [0x10, 0x08, 0x04, 0x02, 0x04, 0x08, 0x10]
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
    slot <- readIORef (uiCurrentSlot ui)
    saveReq <- readIORef (hkSaveReq hk)
    when saveReq $ do
        writeIORef (hkSaveReq hk) False
        blob <- Snap.save machine
        let path = slotPath romPath slot
        r <- try (createDirectoryIfMissing True (romDir romPath) >> BS.writeFile path blob) :: IO (Either IOException ())
        case r of
            Right () -> do
                putStrLn ("state:    saved " <> path <> " (" <> show (BS.length blob) <> " B)")
                pushToast ui ToastSuccess ("Saved slot " <> show slot)
            Left e -> do
                putStrLn ("state:    save failed: " <> show e)
                pushToast ui ToastFailure "Save failed"
    loadReq <- readIORef (hkLoadReq hk)
    when loadReq $ do
        writeIORef (hkLoadReq hk) False
        let path = slotPath romPath slot
        r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
        case r of
            Right blob -> do
                res <- Snap.load blob machine
                case res of
                    Right () -> do
                        putStrLn ("state:    loaded " <> path)
                        pushToast ui ToastSuccess ("Loaded slot " <> show slot)
                    Left err -> do
                        putStrLn ("state:    load failed: " <> show err)
                        pushToast ui ToastFailure "Load failed"
            Left _ -> do
                putStrLn ("state:    no " <> path <> " to load")
                pushToast ui ToastFailure ("Slot " <> show slot <> " empty")
    shotReq <- readIORef (hkShotReq hk)
    when shotReq $ do
        writeIORef (hkShotReq hk) False
        fb <- Ppu.framebufferRgbBytes (Bus.busPpu (machineBus machine))
        ts <- floor <$> getPOSIXTime :: IO Int
        let path = romDir romPath </> ("screenshot-" <> show ts <> ".ppm")
        r <- try (createDirectoryIfMissing True (romDir romPath) >> writePpm path fb) :: IO (Either IOException ())
        case r of
            Right () -> do
                putStrLn ("shot:     wrote " <> path)
                pushToast ui ToastSuccess "Screenshot written"
            Left e -> do
                putStrLn ("shot:     failed: " <> show e)
                pushToast ui ToastFailure "Screenshot failed"
    gifToggle <- readIORef (hkGifToggle hk)
    when gifToggle $ do
        writeIORef (hkGifToggle hk) False
        mFrames <- readIORef (uiGifFrames ui)
        case mFrames of
            Nothing -> do
                writeIORef (uiGifFrames ui) (Just [])
                putStrLn "gif:      recording started"
                pushToast ui ToastInfo "Recording GIF..."
            Just capturedFrames -> do
                writeIORef (uiGifFrames ui) Nothing
                let n = length capturedFrames
                if n == 0
                    then pushToast ui ToastInfo "No frames recorded"
                    else do
                        ts <- floor <$> getPOSIXTime :: IO Int
                        let path = romDir romPath </> ("recording-" <> show ts <> ".gif")
                            opts =
                                PaletteOptions
                                    { paletteCreationMethod = MedianMeanCut
                                    , enableImageDithering = False
                                    , paletteColorCount = 64 -- GB games use ≤56 colours
                                    }
                            -- palettize returns (Image Pixel8, Palette); writeGifImages wants (Palette, GifDelay, Image Pixel8)
                            -- 3cs delay matches the every-other-frame capture rate (≈33 fps)
                            images =
                                [ (pal, 3 :: GifDelay, idx)
                                | f <- reverse capturedFrames
                                , let (idx, pal) = palettize opts (toGifImage f)
                                ]
                        case writeGifImages path LoopingForever images of
                            Left err -> do
                                putStrLn ("gif:      encode failed: " <> err)
                                pushToast ui ToastFailure "GIF encode failed"
                            Right writeAction -> do
                                r <- try (createDirectoryIfMissing True (romDir romPath) >> writeAction) :: IO (Either IOException ())
                                case r of
                                    Right () -> do
                                        putStrLn ("gif:      wrote " <> path <> " (" <> show n <> " frames)")
                                        pushToast ui ToastSuccess "GIF saved"
                                    Left e -> do
                                        putStrLn ("gif:      write failed: " <> show e)
                                        pushToast ui ToastFailure "GIF write failed"
    resetReq <- readIORef (hkResetReq hk)
    when resetReq $ do
        writeIORef (hkResetReq hk) False
        machine' <- machineFromCartridgeWithBoot bootRom cart
        writeIORef machineRef machine'
        putStrLn "reset:    machine rebuilt from cartridge"
        pushToast ui ToastInfo "Machine reset"

toGifImage :: V.Vector Word8 -> Image PixelRGB8
toGifImage fb = generateImage pixel gbWidth gbHeight
  where
    pixel x y =
        let i = (y * gbWidth + x) * 3
         in PixelRGB8 (fb V.! i) (fb V.! (i + 1)) (fb V.! (i + 2))

writePpm :: FilePath -> BS.ByteString -> IO ()
writePpm path fb = do
    let header =
            BS.pack
                ( map (fromIntegral . fromEnum) $
                    "P6\n" <> show gbWidth <> " " <> show gbHeight <> "\n255\n"
                )
    BS.writeFile path (header <> fb)

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
            mods = SDL.keysymModifier (SDL.keyboardEventKeysym kev)
            shiftHeld = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
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
                when (pressed && not isRepeat) $
                    if shiftHeld
                        then writeIORef (hkGifToggle hk) True
                        else writeIORef (hkShotReq hk) True
            SDL.ScancodePeriod ->
                when (pressed && not isRepeat) $ do
                    helpVisible <- readIORef (uiHelpVisible ui)
                    unless helpVisible (writeIORef (hkFrameStepReq hk) True)
            SDL.ScancodeR ->
                when (pressed && not isRepeat) (writeIORef (hkResetReq hk) True)
            SDL.ScancodeF11 ->
                when (pressed && not isRepeat) (writeIORef (hkFullscreenReq hk) True)
            SDL.ScancodeP ->
                when (pressed && not isRepeat) $
                    modifyIORef' (uiPerfVisible ui) not
            SDL.ScancodeF6 ->
                when (pressed && not isRepeat) $ do
                    s <- readIORef (uiCurrentSlot ui)
                    let s' = s `mod` 5 + 1
                    writeIORef (uiCurrentSlot ui) s'
                    pushToast ui ToastInfo ("Slot " <> show s')
            SDL.ScancodeO ->
                when (pressed && not isRepeat) $ do
                    writeIORef (hkOpenReq hk) True
                    writeIORef (hkQuit hk) True
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
updateTextureRgb :: SDL.Texture -> BS.ByteString -> IO ()
updateTextureRgb tex fb = do
    _ <- SDL.updateTexture tex Nothing fb (fromIntegral (gbWidth * 3))
    pure ()

{- | Open a native OS file picker for ROM files.  Returns 'Nothing' if no
suitable tool is available or the user cancels.
-}
showOpenFileDialog :: IO (Maybe FilePath)
showOpenFileDialog = do
    r <- try runDialog :: IO (Either SomeException String)
    pure $ case r of
        Left _ -> Nothing
        Right s ->
            let p = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
             in if null p then Nothing else Just p
  where
#ifdef mingw32_HOST_OS
    runDialog =
        readProcess
            "powershell"
            [ "-NoProfile"
            , "-NonInteractive"
            , "-Command"
            , "[void][System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms');"
                <> "$d=New-Object System.Windows.Forms.OpenFileDialog;"
                <> "$d.Title='Open ROM';"
                <> "$d.Filter='GB/GBC ROMs|*.gb;*.gbc;*.sgb;*.zip|All Files|*.*';"
                <> "if($d.ShowDialog()-eq'OK'){$d.FileName}"
            ]
            ""
#elif defined(darwin_HOST_OS)
    runDialog =
        readProcess
            "osascript"
            ["-e", "POSIX path of (choose file with prompt \"Open ROM\" of type {\"gb\", \"gbc\", \"sgb\", \"zip\"})"]
            ""
#else
    runDialog = do
        mZenity <- findExecutable "zenity"
        case mZenity of
            Just exe -> do
                (code, out, _) <-
                    readProcessWithExitCode
                        exe
                        ["--file-selection", "--title=Open ROM", "--file-filter=GB/GBC ROMs | *.gb *.gbc *.sgb *.zip"]
                        ""
                pure $ case code of { ExitSuccess -> out; _ -> "" }
            Nothing -> do
                mKdialog <- findExecutable "kdialog"
                case mKdialog of
                    Just exe -> do
                        (code, out, _) <-
                            readProcessWithExitCode
                                exe
                                ["--getopenfilename", ".", "GB/GBC ROMs (*.gb *.gbc *.sgb *.zip)"]
                                ""
                        pure $ case code of { ExitSuccess -> out; _ -> "" }
                    Nothing -> pure ""
#endif

{- | Show a startup menu before any ROM is loaded. Returns the path of the ROM the user dropped, or
'Nothing' if the user chose to quit. Opens its own SDL window (title-bar only, no game texture).
-}
startupScreen :: Int -> IO (Maybe FilePath)
startupScreen scale = do
    let (winW0, winH0) = windowSize scale
    SDL.initialize [SDL.InitVideo, SDL.InitEvents]
    let buildTag = T.pack $(gitBranch) <> "@" <> T.pack (take 7 $(gitHash))
    window <-
        SDL.createWindow
            ("Ocelot Emulator (" <> buildTag <> ")")
            SDL.defaultWindow
                { SDL.windowInitialSize = SDL.V2 (fromIntegral winW0) (fromIntegral winH0)
                , SDL.windowResizable = True
                }
    (renderer, paceMode) <- createRendererWithPacing window
    SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
    selRef <- newIORef (0 :: Int)
    waitRef <- newIORef False
    result <- startupLoop renderer paceMode window selRef waitRef
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    pure result

startupLoop :: SDL.Renderer -> PaceMode -> SDL.Window -> IORef Int -> IORef Bool -> IO (Maybe FilePath)
startupLoop renderer paceMode window selRef waitRef = do
    events <- SDL.pollEvents
    mDecision <- processStartupEvents selRef waitRef events
    case mDecision of
        Just decision -> pure decision
        Nothing -> do
            SDL.V2 wW wH <- SDL.get (SDL.windowSize window)
            let winW = fromIntegral wW :: Int
                winH = fromIntegral wH :: Int
            SDL.rendererDrawColor renderer $= SDL.V4 0x08 0x0E 0x08 0xFF
            SDL.clear renderer
            sel <- readIORef selRef
            waiting <- readIORef waitRef
            renderStartupUi renderer winW winH sel waiting
            SDL.present renderer
            when (paceMode == PaceSleep) (threadDelay 16000)
            startupLoop renderer paceMode window selRef waitRef

processStartupEvents :: IORef Int -> IORef Bool -> [SDL.Event] -> IO (Maybe (Maybe FilePath))
processStartupEvents selRef waitRef = go
  where
    menuLen = 2 :: Int
    go [] = pure Nothing
    go (ev : rest) = case SDL.eventPayload ev of
        SDL.QuitEvent -> pure (Just Nothing)
        SDL.DropEvent dd -> do
            fp <- peekCString (SDL.dropEventFile dd)
            pure (Just (Just fp))
        SDL.KeyboardEvent kev
            | SDL.keyboardEventKeyMotion kev == SDL.Pressed
            , not (SDL.keyboardEventRepeat kev) ->
                case SDL.keysymScancode (SDL.keyboardEventKeysym kev) of
                    SDL.ScancodeEscape -> do
                        waiting <- readIORef waitRef
                        if waiting
                            then do writeIORef waitRef False; go rest
                            else pure (Just Nothing)
                    SDL.ScancodeUp -> do
                        waiting <- readIORef waitRef
                        unless waiting $ modifyIORef' selRef (\s -> (s - 1 + menuLen) `mod` menuLen)
                        go rest
                    SDL.ScancodeDown -> do
                        waiting <- readIORef waitRef
                        unless waiting $ modifyIORef' selRef (\s -> (s + 1) `mod` menuLen)
                        go rest
                    SDL.ScancodeReturn -> do
                        waiting <- readIORef waitRef
                        if waiting
                            then go rest
                            else do
                                s <- readIORef selRef
                                case s of
                                    0 -> do
                                        mPath <- showOpenFileDialog
                                        case mPath of
                                            Just path -> pure (Just (Just path))
                                            Nothing -> do writeIORef waitRef True; go rest
                                    1 -> pure (Just Nothing)
                                    _ -> go rest
                    _ -> go rest
        _ -> go rest

renderStartupUi :: SDL.Renderer -> Int -> Int -> Int -> Bool -> IO ()
renderStartupUi renderer winW winH sel waiting = do
    let logoScale = 3
        logoText = "OCELOT"
        logoW = textWidth logoScale logoText
        logoGlyphH = 7 * logoScale
        menuItems = ["OPEN ROM", "QUIT"] :: [String]
        rowH = 7 * 2 + 12
        menuH = length menuItems * rowH
        hintText
            | waiting = "DROP A ROM FILE ON THIS WINDOW"
            | otherwise = "DROP A ROM FILE TO START"
        hintW = textWidth 2 hintText
        panelW = min (winW - 40) (max (hintW + 28) 280)
        panelH = 20 + logoGlyphH + 16 + 2 + 12 + (7 * 2) + 20 + menuH + 20
        panelX = (winW - panelW) `div` 2
        panelY = (winH - panelH) `div` 2
        borderAccent = if waiting then accentBlue else accentOrange
    drawPanel renderer panelPrimary borderAccent panelX panelY panelW panelH
    -- Logo
    let logoX = panelX + (panelW - logoW) `div` 2
        logoY = panelY + 20
    drawTextShadowed renderer logoScale accentOrange logoX logoY logoText
    -- Divider
    let divY = logoY + logoGlyphH + 8
    fillRect renderer accentOrange (panelX + 14) divY (panelW - 28) 2
    -- Hint
    let hintX = panelX + (panelW - hintW) `div` 2
        hintY = divY + 12
        hintColor = if waiting then accentBlue else textSecondary
    drawTextShadowed renderer 2 hintColor hintX hintY hintText
    -- Menu
    let menuStartY = hintY + 7 * 2 + 20
    forM_ (zip [0 ..] menuItems) $ \(i, item) -> do
        let itemY = menuStartY + i * rowH
            isSelected = i == sel && not waiting
            (cursor, textColor)
                | isSelected = ("> ", accentOrange)
                | otherwise = ("  ", if waiting then textMuted else textPrimary)
            itemText = cursor <> item
            itemW = textWidth 2 itemText
            itemX = panelX + (panelW - itemW) `div` 2
        when isSelected $
            fillRect renderer panelSecondary (panelX + 4) (itemY - 4) (panelW - 8) (7 * 2 + 8)
        drawTextShadowed renderer 2 textColor itemX itemY itemText

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
    audioBuf <- newMVar (Seq.fromList samples)
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
