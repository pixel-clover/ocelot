{-# LANGUAGE BangPatterns #-}

{- | Browser-friendly emulator session helpers.

This module keeps the desktop SDL frontend out of the loop and exposes the
operations a web host needs: load a ROM from bytes, run one LCD frame,
push joypad input, read the RGB framebuffer, drain audio samples, and
persist save-state or battery-backed RAM blobs.
-}
module Ocelot.Web (
    WebSession,
    loadSession,
    runFrame,
    stalledFrames,
    stallThreshold,
    debugState,
    setButton,
    framebufferRgb,
    framebufferRgbBytes,
    framebufferRgbaBytes,
    framebufferRgbaPtr,
    copyFramebufferRgba,
    drainAudioSamples,
    drainAudioSamplesVector,
    drainAudioSamplesInto,
    saveState,
    loadState,
    extractSaveData,
    loadSaveData,
    setFbTargetRgba,
    sessionTitle,
    sessionHasBattery,
    sessionIsCgb,
    framebufferWidth,
    framebufferHeight,
    audioSampleRate,
) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import qualified Ocelot.Apu as Apu
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Cpu.Execute (runUntilFrame)
import Ocelot.Joypad (Button)
import Ocelot.Machine (Machine (..), debugSummary, machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot as Snapshot

data WebSession = WebSession
    { wsMachine :: !Machine
    , wsCartridge :: !Cartridge.Cartridge
    , wsTitle :: !Text
    , wsHasBattery :: !Bool
    , wsFbFingerprint :: !(IORef Word32)
    , wsStalledFrames :: !(IORef Int)
    }

loadSession :: ByteString -> IO (Either Cartridge.CartridgeError WebSession)
loadSession romBytes = do
    loaded <- Cartridge.loadRom romBytes
    case loaded of
        Left err -> pure (Left err)
        Right cart -> do
            machine <- machineFromCartridge cart
            fingerprint <- newIORef 0
            stalled <- newIORef 0
            pure $
                Right
                    WebSession
                        { wsMachine = machine
                        , wsCartridge = cart
                        , wsTitle = Header.hdrTitle (Cartridge.cartridgeHeader cart)
                        , wsHasBattery = Cartridge.cartridgeHasBattery cart
                        , wsFbFingerprint = fingerprint
                        , wsStalledFrames = stalled
                        }

runFrame :: WebSession -> IO ()
runFrame session = do
    frameCycles <- Bus.cpuMCyclesPerLcdFrame (machineBus (wsMachine session))
    _ <- runUntilFrame (frameCycles + 32) (wsMachine session)
    updateStallWatchdog session

{- | Consecutive frames whose picture was byte-identical to the frame before.

A host polls this once per frame; it is a plain counter read, so it costs nothing to
check. Crossing 'stallThreshold' is the cue to fetch 'debugState' and report it. The
counter resets to 0 the moment the picture changes, so a host that reports on the
crossing reports once per stall episode rather than once per frame.
-}
stalledFrames :: WebSession -> IO Int
stalledFrames = readIORef . wsStalledFrames

{- | Frames of unchanging picture that count as a stall: about three seconds.

A still picture is not the same thing as a hang, so this can never be treated as an
error; it is a cue to gather diagnostics. The first version used ten seconds to keep
menus quiet, which turned out to be too long for the symptom that matters: a game that
freezes and then restarts is only still for a moment, and a ten-second threshold never
fires at all.

Three seconds is the compromise. It is longer than any brief pause during play and short
enough to catch a freeze that ends in a restart. Menus and title screens do trip it, but
the host latches the report so one still stretch yields one console line, which is a
tolerable price for catching the real thing.
-}
stallThreshold :: Int
stallThreshold = 180

{- | Machine state plus how long the picture has been still, as reportable text.

Pairs 'Ocelot.Machine.debugSummary' with the watchdog's own counter. Intended to be
handed to the user verbatim: it is the difference between "it froze" and a bug report
someone can act on.
-}
debugState :: WebSession -> IO ByteString
debugState session = do
    summary <- debugSummary (wsMachine session)
    stalled <- readIORef (wsStalledFrames session)
    pure (summary <> BSC.pack (" stalledFrames=" <> show stalled))

{- | Fold this frame's picture into the stall counter.

The fingerprint samples the RGBA framebuffer rather than hashing all 92160 bytes,
which keeps the per-frame cost negligible. 'fingerprintStride' is coprime with the
4-byte pixel stride so the samples walk across all four channels instead of landing
on the same one every time.

This reads the RGBA buffer, so it only tracks a host that has left 'FbRgba' or
'FbBoth' selected. A host that switches to 'FbRgb' alone leaves RGBA frozen and would
see a permanent false stall; the web host sets RGBA, which is what this is for.
-}
updateStallWatchdog :: WebSession -> IO ()
updateStallWatchdog session = do
    current <- fingerprintFramebuffer session
    previous <- readIORef (wsFbFingerprint session)
    writeIORef (wsFbFingerprint session) current
    if current == previous
        then do
            n <- readIORef (wsStalledFrames session)
            writeIORef (wsStalledFrames session) (n + 1)
        else writeIORef (wsStalledFrames session) 0

fingerprintStride :: Int
fingerprintStride = 61

fingerprintFramebuffer :: WebSession -> IO Word32
fingerprintFramebuffer session = go 0 2166136261
  where
    ptr = framebufferRgbaPtr session
    total = framebufferWidth * framebufferHeight * 4
    go !i !h
        | i >= total = pure h
        | otherwise = do
            byte <- peekByteOff ptr i :: IO Word8
            go (i + fingerprintStride) ((h `xor` fromIntegral byte) * 16777619)

setButton :: Button -> Bool -> WebSession -> IO ()
setButton button pressed session =
    Bus.setButton button pressed (machineBus (wsMachine session))

framebufferRgb :: WebSession -> IO (V.Vector Word8)
framebufferRgb session =
    Bus.framebufferRgb (machineBus (wsMachine session))

framebufferRgbBytes :: WebSession -> IO ByteString
framebufferRgbBytes session =
    Bus.framebufferRgbBytes (machineBus (wsMachine session))

framebufferRgbaBytes :: WebSession -> IO ByteString
framebufferRgbaBytes session =
    Bus.framebufferRgbaBytes (machineBus (wsMachine session))

copyFramebufferRgba :: Ptr Word8 -> WebSession -> IO ()
copyFramebufferRgba ptr session =
    Bus.copyFramebufferRgba ptr (machineBus (wsMachine session))

{- | Stable pointer directly into the RGBA framebuffer. Valid for the
lifetime of the 'WebSession'. Allows the WASM host to read the framebuffer
without an intermediate copy.
-}
framebufferRgbaPtr :: WebSession -> Ptr Word8
framebufferRgbaPtr session =
    Bus.framebufferRgbaPtr (machineBus (wsMachine session))

drainAudioSamples :: WebSession -> IO [Int16]
drainAudioSamples session =
    Bus.drainAudioSamples (machineBus (wsMachine session))

drainAudioSamplesVector :: WebSession -> IO (V.Vector Int16)
drainAudioSamplesVector session =
    Bus.drainAudioSamplesVector (machineBus (wsMachine session))

drainAudioSamplesInto :: Ptr Int16 -> Int -> WebSession -> IO Int
drainAudioSamplesInto ptr capacity session =
    Bus.drainAudioSamplesInto ptr capacity (machineBus (wsMachine session))

saveState :: WebSession -> IO ByteString
saveState = Snapshot.save . wsMachine

loadState :: ByteString -> WebSession -> IO (Either Snapshot.SnapshotError ())
loadState blob session = Snapshot.load blob (wsMachine session)

extractSaveData :: WebSession -> IO ByteString
extractSaveData = Cartridge.extractSave . wsCartridge

loadSaveData :: ByteString -> WebSession -> IO ()
loadSaveData blob session = Cartridge.loadSave blob (wsCartridge session)

sessionTitle :: WebSession -> Text
sessionTitle = wsTitle

sessionHasBattery :: WebSession -> Bool
sessionHasBattery = wsHasBattery

sessionIsCgb :: WebSession -> Bool
sessionIsCgb session = Bus.isCgb (machineBus (wsMachine session))

{- | Switch the PPU to write only the RGBA framebuffer. Call this once after
'loadSession' in a host that reads the RGBA buffer exclusively (e.g. the
WASM frontend), to skip the unused RGB writes each scanline.
-}
setFbTargetRgba :: WebSession -> IO ()
setFbTargetRgba session =
    Ppu.setFbTarget Ppu.FbRgba (Bus.busPpu (machineBus (wsMachine session)))

framebufferWidth, framebufferHeight :: Int
framebufferWidth = Ppu.framebufferWidth
framebufferHeight = Ppu.framebufferHeight

audioSampleRate :: Int
audioSampleRate = Apu.sampleRate
