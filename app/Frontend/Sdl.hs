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
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Apu as Apu
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge)
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Joypad (Button (..), JoypadState)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot as Snap
import qualified SDL

gbWidth, gbHeight :: Int
gbWidth = 160
gbHeight = 144

scale :: Int
scale = 4

mCyclesPerFrame :: Int
mCyclesPerFrame = 17556

frameUs :: Int
frameUs = 16667

{- | Cap the audio buffer so an emulator running ahead of the audio device
doesn't accumulate unbounded samples.
-}
maxBufferedSamples :: Int
maxBufferedSamples = 9600 -- ~100 ms of stereo samples at 48 kHz


data Hotkeys = Hotkeys
    { hkQuit :: !(IORef Bool)
    , hkPaused :: !(IORef Bool)
    , hkFastFwd :: !(IORef Bool)
    , hkSaveReq :: !(IORef Bool)
    , hkLoadReq :: !(IORef Bool)
    , hkShotReq :: !(IORef Bool)
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

play :: FilePath -> Cartridge -> Text -> IO ()
play romPath cart title = do
    SDL.initialize [SDL.InitVideo, SDL.InitEvents, SDL.InitAudio]
    window <-
        SDL.createWindow
            ("Ocelot - " <> title)
            SDL.defaultWindow
                { SDL.windowInitialSize =
                    SDL.V2 (fromIntegral (gbWidth * scale)) (fromIntegral (gbHeight * scale))
                }
    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.defaultRenderer{SDL.rendererType = SDL.AcceleratedVSyncRenderer}
    texture <-
        SDL.createTexture
            renderer
            SDL.RGB24
            SDL.TextureAccessStreaming
            (SDL.V2 (fromIntegral gbWidth) (fromIntegral gbHeight))

    audioBuf <- newMVar []
    audioDev <- openAudio audioBuf

    machine <- machineFromCartridge cart
    hk <- newHotkeys

    SDL.setAudioDevicePlaybackState audioDev SDL.Play

    loop romPath hk machine renderer texture audioBuf

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

{- | Audio callback, invoked by SDL's audio thread when it needs more
samples. Drains up to @VSM.length out@ samples from the shared buffer,
padding with silence if the producer is behind.
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
    mapM_ (\(i, s) -> VSM.write out i s) (zip [0 ..] samples)

loop ::
    FilePath ->
    Hotkeys ->
    Machine ->
    SDL.Renderer ->
    SDL.Texture ->
    MVar [Int16] ->
    IO ()
loop romPath hk machine renderer texture audioBuf = do
    quit <- readIORef (hkQuit hk)
    unless quit $ do
        events <- SDL.pollEvents
        mapM_ (handleEvent hk (Bus.busJoypad (machineBus machine))) events

        -- One-shot hotkeys: handle save/load/screenshot requests.
        handlePending romPath hk machine

        paused <- readIORef (hkPaused hk)
        fast <- readIORef (hkFastFwd hk)
        let frames = if fast then 4 else 1 :: Int

        unless paused $ do
            mapM_ (\_ -> runFor mCyclesPerFrame machine) [1 .. frames]

            samples <- Bus.drainAudioSamples (machineBus machine)
            when (not (null samples)) $
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

        fbRgb <- Ppu.framebufferRgb (Bus.busPpu (machineBus machine))
        updateTextureRgb texture fbRgb
        SDL.clear renderer
        SDL.copy renderer texture Nothing Nothing
        SDL.present renderer

        -- Pace to 60 FPS unless fast-forwarding.
        unless fast (threadDelay frameUs)
        loop romPath hk machine renderer texture audioBuf

handlePending :: FilePath -> Hotkeys -> Machine -> IO ()
handlePending romPath hk machine = do
    saveReq <- readIORef (hkSaveReq hk)
    when saveReq $ do
        writeIORef (hkSaveReq hk) False
        blob <- Snap.save machine
        let path = romPath <> ".state"
        r <- try (BS.writeFile path blob) :: IO (Either IOException ())
        case r of
            Right () -> putStrLn ("state:    saved " <> path <> " (" <> show (BS.length blob) <> " B)")
            Left e -> putStrLn ("state:    save failed: " <> show e)
    loadReq <- readIORef (hkLoadReq hk)
    when loadReq $ do
        writeIORef (hkLoadReq hk) False
        let path = romPath <> ".state"
        r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
        case r of
            Right blob -> do
                res <- Snap.load blob machine
                case res of
                    Right () -> putStrLn ("state:    loaded " <> path)
                    Left err -> putStrLn ("state:    load failed: " <> show err)
            Left _ -> putStrLn ("state:    no " <> path <> " to load")
    shotReq <- readIORef (hkShotReq hk)
    when shotReq $ do
        writeIORef (hkShotReq hk) False
        fb <- Ppu.framebufferRgb (Bus.busPpu (machineBus machine))
        ts <- floor <$> getPOSIXTime :: IO Int
        let path = romPath <> "-" <> show ts <> ".ppm"
        r <- try (writePpm path fb) :: IO (Either IOException ())
        case r of
            Right () -> putStrLn ("shot:     wrote " <> path)
            Left e -> putStrLn ("shot:     failed: " <> show e)

writePpm :: FilePath -> V.Vector Word8 -> IO ()
writePpm path fb = do
    let header =
            BS.pack
                ( map (fromIntegral . fromEnum) $
                    "P6\n" <> show gbWidth <> " " <> show gbHeight <> "\n255\n"
                )
        body = BS.pack (V.toList fb)
    BS.writeFile path (header <> body)

handleEvent :: Hotkeys -> JoypadState -> SDL.Event -> IO ()
handleEvent hk jp ev = case SDL.eventPayload ev of
    SDL.QuitEvent -> writeIORef (hkQuit hk) True
    SDL.KeyboardEvent kev -> do
        let pressed = SDL.keyboardEventKeyMotion kev == SDL.Pressed
            scancode = SDL.keysymScancode (SDL.keyboardEventKeysym kev)
            isRepeat = SDL.keyboardEventRepeat kev
        case scancode of
            SDL.ScancodeEscape -> when pressed (writeIORef (hkQuit hk) True)
            SDL.ScancodeSpace ->
                when (pressed && not isRepeat) $
                    modifyIORef' (hkPaused hk) not
            SDL.ScancodeTab -> writeIORef (hkFastFwd hk) pressed
            SDL.ScancodeF5 ->
                when (pressed && not isRepeat) (writeIORef (hkSaveReq hk) True)
            SDL.ScancodeF7 ->
                when (pressed && not isRepeat) (writeIORef (hkLoadReq hk) True)
            SDL.ScancodeF12 ->
                when (pressed && not isRepeat) (writeIORef (hkShotReq hk) True)
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

-- | Streaming-update path: 'fb' is already in RGB888 with one byte per
-- channel, so the SDL upload is a single 'BS.pack' away.
updateTextureRgb :: SDL.Texture -> V.Vector Word8 -> IO ()
updateTextureRgb tex fb = do
    let bs = BS.pack (V.toList fb)
    _ <- SDL.updateTexture tex Nothing bs (fromIntegral (gbWidth * 3))
    pure ()

{- | Diagnostic: open the audio device and play a 440 Hz sine tone for two
seconds, bypassing the APU. If you hear nothing here, the SDL audio path is
the problem; if you hear the tone, the APU is the problem.
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
