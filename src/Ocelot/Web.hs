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
    setButton,
    framebufferRgb,
    framebufferRgbBytes,
    framebufferRgbaBytes,
    drainAudioSamples,
    saveState,
    loadState,
    extractSaveData,
    loadSaveData,
    sessionTitle,
    sessionHasBattery,
    sessionIsCgb,
    framebufferWidth,
    framebufferHeight,
    audioSampleRate,
) where

import Data.ByteString (ByteString)
import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Apu as Apu
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Joypad (Button)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot as Snapshot

data WebSession = WebSession
    { wsMachine :: !Machine
    , wsCartridge :: !Cartridge.Cartridge
    , wsTitle :: !Text
    , wsHasBattery :: !Bool
    }

loadSession :: ByteString -> IO (Either Cartridge.CartridgeError WebSession)
loadSession romBytes = do
    loaded <- Cartridge.loadRom romBytes
    case loaded of
        Left err -> pure (Left err)
        Right cart -> do
            machine <- machineFromCartridge cart
            pure $
                Right
                    WebSession
                        { wsMachine = machine
                        , wsCartridge = cart
                        , wsTitle = Header.hdrTitle (Cartridge.cartridgeHeader cart)
                        , wsHasBattery = Cartridge.cartridgeHasBattery cart
                        }

runFrame :: WebSession -> IO ()
runFrame session = do
    frameCycles <- Bus.cpuMCyclesPerLcdFrame (machineBus (wsMachine session))
    _ <- runFor frameCycles (wsMachine session)
    pure ()

setButton :: Button -> Bool -> WebSession -> IO ()
setButton button pressed session =
    Joypad.setButton button pressed (Bus.busJoypad (machineBus (wsMachine session)))

framebufferRgb :: WebSession -> IO (V.Vector Word8)
framebufferRgb session =
    Ppu.framebufferRgb (Bus.busPpu (machineBus (wsMachine session)))

framebufferRgbBytes :: WebSession -> IO ByteString
framebufferRgbBytes session =
    Ppu.framebufferRgbBytes (Bus.busPpu (machineBus (wsMachine session)))

framebufferRgbaBytes :: WebSession -> IO ByteString
framebufferRgbaBytes session =
    Ppu.framebufferRgbaBytes (Bus.busPpu (machineBus (wsMachine session)))

drainAudioSamples :: WebSession -> IO [Int16]
drainAudioSamples session =
    Bus.drainAudioSamples (machineBus (wsMachine session))

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
sessionIsCgb session = Bus.busCgb (machineBus (wsMachine session))

framebufferWidth, framebufferHeight :: Int
framebufferWidth = Ppu.framebufferWidth
framebufferHeight = Ppu.framebufferHeight

audioSampleRate :: Int
audioSampleRate = Apu.sampleRate
