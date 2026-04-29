{-# LANGUAGE BangPatterns #-}

{- | The system bus.

Routes 16-bit addresses to the right backing store: cartridge ROM and ERAM,
WRAM (with echo mirror), VRAM and OAM (owned by the PPU), IO registers,
HRAM, and the IE byte.

State is held in 'IORef's and 'IOVector's, so the public API is in @IO@.
Reads and writes are O(1) and the framebuffer / WRAM / HRAM updates do not
copy.

Special handling on writes:

* Writes to the cartridge ROM window @0x0000-0x7FFF@ are forwarded to
  'Ocelot.Cartridge.write8'.
* Writes to the serial control register @0xFF02@ that initiate a transfer
  capture the byte at @0xFF01@ into the serial output buffer and clear the
  start bit.
* Writes to @0xFF46@ trigger an immediate OAM DMA copying 160 bytes from
  @(v << 8)@ into OAM.
* The unusable region @0xFEA0-0xFEFF@ ignores writes; reads return @0xFF@.
* @0xFF00@ (joypad) returns "no buttons pressed" and lets row-select round-trip.
-}
module Ocelot.Bus (
    Bus (..),
    fromCartridge,
    read8,
    write8,
    advance,
    drainSerial,
    drainAudioSamples,
    triggerSpeedSwitch,
    installBootRom,
) where

import Control.Monad (replicateM_, when)
import Data.Bits (setBit, shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
import Ocelot.Apu (ApuState)
import qualified Ocelot.Apu as Apu
import Ocelot.Cartridge (Cartridge)
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Joypad (JoypadState)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Ppu (PpuState)
import qualified Ocelot.Ppu as Ppu
import Ocelot.Timer (TimerState)
import qualified Ocelot.Timer as Timer

data Bus = Bus
    { busCart :: !Cartridge
    , busWram :: !(IOVector Word8)
    -- ^ 32 KiB on hardware (8 banks of 4 KiB). DMG only uses banks 0
    -- and 1; CGB games select bank 1..7 for the upper 4 KiB via
    -- 'busWramBank' (0xFF70). Bank 0 is always the lower 4 KiB.
    , busHram :: !(IOVector Word8)
    , busIo :: !(IOVector Word8)
    , busIe :: !(IORef Word8)
    , busTimer :: !(IORef TimerState)
    , busPpu :: !PpuState
    , busApu :: !ApuState
    , busJoypad :: !JoypadState
    , busSerialOut :: !(IORef [Word8])
    , busCgb :: !Bool
    -- ^ True when the loaded cartridge is CGB-aware ('DmgAndCgb' or
    -- 'CgbOnly'). Gates CGB-only registers (VBK, BCPS, etc.).
    , busWramBank :: !(IORef Word8)
    -- ^ CGB WRAM bank select (0xFF70). Low 3 bits select banks 1..7;
    -- bank 0 is treated as bank 1 on real hardware.
    , busKey1 :: !(IORef Word8)
    -- ^ CGB KEY1 (0xFF4D) speed-switch register stub. Bit 0 = "prepare
    -- switch" (writable); bit 7 = current speed. Live double-speed is
    -- toggled via 'triggerSpeedSwitch' on the @STOP@ instruction.
    , busHdmaSrc :: !(IORef Word16)
    -- ^ HDMA source address (HDMA1\/HDMA2). Aligned to 16 bytes; the
    -- low 4 bits are forced to 0 on write.
    , busHdmaDst :: !(IORef Word16)
    -- ^ HDMA destination address (HDMA3\/HDMA4). Constrained to the
    -- VRAM window @0x8000-0x9FFF@; low 4 bits forced to 0.
    , busHdmaLen :: !(IORef Int)
    -- ^ Bytes remaining in the current HDMA transfer (multiple of 16).
    -- Zero when no transfer is in progress.
    , busHdmaActive :: !(IORef Bool)
    -- ^ True while an HBlank-mode HDMA is pending more chunks.
    , busDoubleSpeed :: !(IORef Bool)
    -- ^ Whether the CGB is running in double-speed mode. Set by 'stop'
    -- when KEY1 bit 0 (prepare switch) is high; toggled bit 7 of KEY1.
    -- Peripherals see only half as many M-cycles per 'advance' so they
    -- continue to tick at wall-clock rate.
    , busDoubleSpeedAcc :: !(IORef Int)
    -- ^ 0\/1 accumulator for odd peripheral M-cycles in double-speed.
    , busBootRom :: !(IORef (Maybe ByteString))
    -- ^ Optional boot ROM. When 'Just', addresses @0x0000-0x00FF@
    -- (and on CGB: also @0x0200-0x08FF@) are served from this byte
    -- string instead of the cartridge ROM, until @0xFF50@ is written
    -- with a non-zero value.
    , busBootRomActive :: !(IORef Bool)
    -- ^ Locked flag for the boot ROM. Initialized to True when a boot
    -- ROM is installed; set to False permanently after the first
    -- @0xFF50@ write with bit 0 set (which is what the boot ROM does
    -- as its last action before handing off to the cartridge).
    , busHardwareCgb :: !Bool
    -- ^ Whether we model a CGB host. DMG-only carts on a CGB host run
    -- in CGB-compatibility mode (auto-colorized via CGB palette RAM)
    -- instead of greenish-DMG. The SDL frontend defaults to True;
    -- a future flag can opt back to a pure DMG host.
    , busOamDmaActive :: !(IORef Bool)
    -- ^ True while an OAM DMA is in progress (between the M-cycle after
    -- the @0xFF46@ write and the M-cycle the 160th byte is copied).
    -- While active, all CPU reads outside HRAM return @0xFF@.
    , busOamDmaSrc :: !(IORef Word16)
    -- ^ Latched source base address (the FF46 byte shifted left by 8).
    , busOamDmaIndex :: !(IORef Int)
    -- ^ Next OAM offset to copy, 0..160. Reaching 160 deactivates DMA.
    , busOamDmaStarting :: !(IORef Bool)
    -- ^ True for one M-cycle between the FF46 write and the first byte
    -- copy. Models the documented "DMA starts after the cycle in which
    -- it was triggered" behavior.
    }

fromCartridge :: Cartridge -> IO Bus
fromCartridge c = do
    wram <- MV.replicate 0x8000 0
    hram <- MV.replicate 0x7F 0
    io <- MV.replicate 0x80 0
    ie <- newIORef 0
    timer <- newIORef Timer.initialTimer
    ppu <- Ppu.initialPpu
    apu <- Apu.initial
    joypad <- Joypad.initial
    serial <- newIORef []
    wramBank <- newIORef 0x01
    key1 <- newIORef 0x00
    hdmaSrc <- newIORef 0
    hdmaDst <- newIORef 0x8000
    hdmaLen <- newIORef 0
    hdmaActive <- newIORef False
    doubleSpeed <- newIORef False
    doubleSpeedAcc <- newIORef 0
    bootRom <- newIORef Nothing
    bootRomActive <- newIORef False
    oamDmaActive <- newIORef False
    oamDmaSrc <- newIORef 0
    oamDmaIndex <- newIORef 0
    oamDmaStarting <- newIORef False
    let cgbCart = case Header.hdrCgbFlag (Cartridge.cartridgeHeader c) of
            Header.DmgOnly -> False
            Header.DmgAndCgb -> True
            Header.CgbOnly -> True
        hardwareCgb = True -- SDL frontend models a CGB; DMG-only flag is a follow-up
        cgb = cgbCart
        renderMode
            | cgbCart = Ppu.RenderCgbFull
            | hardwareCgb = Ppu.RenderCgbCompat
            | otherwise = Ppu.RenderDmg
    Ppu.setCgbMode cgb ppu
    Ppu.setCgbRenderMode renderMode ppu
    -- APU power-off semantics follow the cart's hardware target rather than
    -- the host: blargg dmg_sound is calibrated for DMG behavior (length
    -- counters preserved), and blargg cgb_sound for CGB behavior (cleared).
    -- Tying the APU mode to the cart lets both suites pass simultaneously.
    Apu.setCgbMode cgb apu
    -- DMG-on-CGB compat: pre-load CGB palette RAM with the auto palette
    -- so the DMG cart's BGP/OBP shades index into recognizable colors.
    when (renderMode == Ppu.RenderCgbCompat) $
        applyCompatPalette (Cartridge.cartridgeHeader c) ppu
    pure
        Bus
            { busCart = c
            , busWram = wram
            , busHram = hram
            , busIo = io
            , busIe = ie
            , busTimer = timer
            , busPpu = ppu
            , busApu = apu
            , busJoypad = joypad
            , busSerialOut = serial
            , busCgb = cgb
            , busWramBank = wramBank
            , busKey1 = key1
            , busHdmaSrc = hdmaSrc
            , busHdmaDst = hdmaDst
            , busHdmaLen = hdmaLen
            , busHdmaActive = hdmaActive
            , busDoubleSpeed = doubleSpeed
            , busDoubleSpeedAcc = doubleSpeedAcc
            , busBootRom = bootRom
            , busBootRomActive = bootRomActive
            , busHardwareCgb = hardwareCgb
            , busOamDmaActive = oamDmaActive
            , busOamDmaSrc = oamDmaSrc
            , busOamDmaIndex = oamDmaIndex
            , busOamDmaStarting = oamDmaStarting
            }

{- | CPU-side bus read. While an OAM DMA is in progress, only HRAM
(@0xFF80-0xFFFE@) and the IE register are accessible to the CPU; all
other addresses return @0xFF@. The DMA itself reads through
'readDmaSource' to bypass this gate.
-}
read8 :: Word16 -> Bus -> IO Word8
read8 addr b = do
    blocked <- readIORef (busOamDmaActive b)
    if blocked && not (addrAccessibleDuringDma addr)
        then pure 0xFF
        else read8Raw addr b

{- | True if the CPU can still read this address while OAM DMA is active.
The CPU is locked off the main memory bus (ROM, VRAM, WRAM, echo, OAM,
unusable region) but can still poke at the I/O register file
(@0xFF00-0xFF7F@), HRAM (@0xFF80-0xFFFE@), and the IE register
(@0xFFFF@) since those sit on a different bus internally.
-}
addrAccessibleDuringDma :: Word16 -> Bool
addrAccessibleDuringDma addr = addr >= 0xFF00

read8Raw :: Word16 -> Bus -> IO Word8
read8Raw addr b
    | addr <= 0x7FFF = bootRomOrCart addr b
    | addr <= 0x9FFF = Ppu.read8 addr (busPpu b)
    | addr <= 0xBFFF = Cartridge.read8 addr (busCart b)
    | addr <= 0xCFFF = MV.read (busWram b) (fromIntegral (addr - 0xC000))
    | addr <= 0xDFFF = readUpperWram (addr - 0xD000) b
    | addr <= 0xFDFF = readEcho (addr - 0xE000) b
    | addr <= 0xFE9F = Ppu.read8 addr (busPpu b)
    | addr <= 0xFEFF = pure 0xFF
    | addr == 0xFF00 = Joypad.readP1 (busJoypad b)
    -- IF (0xFF0F): only the low 5 bits are real interrupt flags; the
    -- upper 3 bits always read as 1.
    | addr == 0xFF0F = (.|. 0xE0) <$> MV.read (busIo b) 0x0F
    | addr == 0xFF04 = Timer.readDiv <$> readIORef (busTimer b)
    | addr == 0xFF05 = Timer.readTima <$> readIORef (busTimer b)
    | addr == 0xFF06 = Timer.readTma <$> readIORef (busTimer b)
    | addr == 0xFF07 = Timer.readTac <$> readIORef (busTimer b)
    | addr >= 0xFF10 && addr <= 0xFF3F = Apu.read8 addr (busApu b)
    -- DMA register (FF46): reads back the last-written source-high byte.
    | addr == 0xFF46 = MV.read (busIo b) 0x46
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.read8 addr (busPpu b)
    | addr == 0xFF4D = readKey1 b
    | addr == 0xFF4F = Ppu.read8 addr (busPpu b)
    | addr == 0xFF55 = readHdma5 b
    | addr == 0xFF68 = Ppu.read8 addr (busPpu b)
    | addr == 0xFF69 = Ppu.read8 addr (busPpu b)
    | addr == 0xFF6A = Ppu.read8 addr (busPpu b)
    | addr == 0xFF6B = Ppu.read8 addr (busPpu b)
    | addr == 0xFF70 = readWramBank b
    | addr <= 0xFF7F = MV.read (busIo b) (fromIntegral (addr - 0xFF00))
    | addr <= 0xFFFE = MV.read (busHram b) (fromIntegral (addr - 0xFF80))
    | otherwise = readIORef (busIe b)

write8 :: Word16 -> Word8 -> Bus -> IO ()
write8 addr !v b = do
    write8Raw addr v b
    -- PPU register writes (STAT, LYC, LCDC bit 7) can drive a low->high
    -- transition of the OR'd STAT line and must raise IF bit 1 right
    -- away. The PPU latches such edges into a pending flag; the bus
    -- consumes it here regardless of which addr was written.
    edge <- Ppu.takePendingStatIrq (busPpu b)
    when edge (setIfBit 1 b)

write8Raw :: Word16 -> Word8 -> Bus -> IO ()
write8Raw addr !v b
    | addr <= 0x7FFF = Cartridge.write8 addr v (busCart b)
    | addr <= 0x9FFF = Ppu.write8 addr v (busPpu b)
    | addr <= 0xBFFF = Cartridge.write8 addr v (busCart b)
    | addr <= 0xCFFF = MV.write (busWram b) (fromIntegral (addr - 0xC000)) v
    | addr <= 0xDFFF = writeUpperWram (addr - 0xD000) v b
    | addr <= 0xFDFF = writeEcho (addr - 0xE000) v b
    | addr <= 0xFE9F = Ppu.write8 addr v (busPpu b)
    | addr <= 0xFEFF = pure ()
    | addr == 0xFF00 = Joypad.writeP1 v (busJoypad b)
    | addr == 0xFF02 = handleSerialControl v b
    | addr == 0xFF04 = modifyIORef' (busTimer b) Timer.writeDiv
    | addr == 0xFF05 = modifyIORef' (busTimer b) (Timer.writeTima v)
    | addr == 0xFF06 = modifyIORef' (busTimer b) (Timer.writeTma v)
    | addr == 0xFF07 = modifyIORef' (busTimer b) (Timer.writeTac v)
    | addr >= 0xFF10 && addr <= 0xFF3F = Apu.write8 addr v (busApu b)
    | addr == 0xFF46 = oamDma v b
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.write8 addr v (busPpu b)
    | addr == 0xFF4D = writeKey1 v b
    | addr == 0xFF4F = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr >= 0xFF51 && addr <= 0xFF55 = writeHdmaReg addr v b
    | addr == 0xFF68 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF69 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6A = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6B = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF50 = writeBootRomLock v b
    | addr == 0xFF70 = writeWramBank v b
    | addr <= 0xFF7F = MV.write (busIo b) (fromIntegral (addr - 0xFF00)) v
    | addr <= 0xFFFE = MV.write (busHram b) (fromIntegral (addr - 0xFF80)) v
    | otherwise = writeIORef (busIe b) v

handleSerialControl :: Word8 -> Bus -> IO ()
handleSerialControl v b
    | testBit v 7 = do
        sb <- MV.read (busIo b) 0x01
        MV.write (busIo b) 0x02 (v .&. 0x7F)
        modifyIORef' (busSerialOut b) (sb :)
    | otherwise = MV.write (busIo b) 0x02 v

{- | Resolve the active upper-WRAM bank: bank 0 is treated as bank 1 on
real hardware, so the lower 4 KiB (always bank 0) is mirrored only when
the selector is 0. Returns the byte offset into 'busWram' for offset
@within = (addr - 0xD000)@.
-}
upperWramOffset :: Word16 -> Bus -> IO Int
upperWramOffset within b = do
    sel <- readIORef (busWramBank b)
    let bank = let n = fromIntegral (sel .&. 0x07) in if n == 0 then 1 else n
    pure (bank * 0x1000 + fromIntegral within)

readUpperWram :: Word16 -> Bus -> IO Word8
readUpperWram within b = do
    off <- upperWramOffset within b
    MV.read (busWram b) off

writeUpperWram :: Word16 -> Word8 -> Bus -> IO ()
writeUpperWram within v b = do
    off <- upperWramOffset within b
    MV.write (busWram b) off v

{- | Echo region @0xE000-0xFDFF@ mirrors @0xC000-0xDDFF@ (the lower 8 KiB
of WRAM, with the upper half routed through the active CGB bank).
-}
readEcho :: Word16 -> Bus -> IO Word8
readEcho within b
    | within < 0x1000 = MV.read (busWram b) (fromIntegral within)
    | otherwise = readUpperWram (within - 0x1000) b

writeEcho :: Word16 -> Word8 -> Bus -> IO ()
writeEcho within v b
    | within < 0x1000 = MV.write (busWram b) (fromIntegral within) v
    | otherwise = writeUpperWram (within - 0x1000) v b

readWramBank :: Bus -> IO Word8
readWramBank b
    | not (busCgb b) = pure 0xFF
    | otherwise = (.|. 0xF8) <$> readIORef (busWramBank b)

writeWramBank :: Word8 -> Bus -> IO ()
writeWramBank v b = when (busCgb b) (writeIORef (busWramBank b) (v .&. 0x07))

{- | KEY1 read: bit 7 = current speed (1 = double-speed), bit 0 =
pending switch. Bits 1..6 read as 1.
-}
readKey1 :: Bus -> IO Word8
readKey1 b
    | not (busCgb b) = pure 0xFF
    | otherwise = do
        prepare <- readIORef (busKey1 b)
        ds <- readIORef (busDoubleSpeed b)
        pure ((if ds then 0x80 else 0x00) .|. (prepare .&. 0x01) .|. 0x7E)

writeKey1 :: Word8 -> Bus -> IO ()
writeKey1 v b = when (busCgb b) (writeIORef (busKey1 b) (v .&. 0x01))

{- | Install a boot ROM. Subsequent reads to the boot-ROM-mapped range
(0x0000-0x00FF on DMG; 0x0000-0x00FF and 0x0200-0x08FF on CGB) come
from this byte string until the cartridge writes a non-zero value to
0xFF50, at which point the boot ROM is unmapped permanently.
-}
installBootRom :: ByteString -> Bus -> IO ()
installBootRom rom b = do
    writeIORef (busBootRom b) (Just rom)
    writeIORef (busBootRomActive b) True

-- | Boot-ROM-aware ROM-window read.
bootRomOrCart :: Word16 -> Bus -> IO Word8
bootRomOrCart addr b = do
    active <- readIORef (busBootRomActive b)
    if active
        then do
            mRom <- readIORef (busBootRom b)
            case mRom of
                Just rom
                    | addrInBootRange (busCgb b) addr ->
                        let i = fromIntegral addr
                         in pure $
                                if i < BS.length rom
                                    then BS.index rom i
                                    else 0xFF
                _ -> Cartridge.read8 addr (busCart b)
        else Cartridge.read8 addr (busCart b)

{- | Whether @addr@ falls in the boot-ROM-mapped range. DMG maps
0x0000-0x00FF; CGB also maps 0x0200-0x08FF (the extended boot region;
0x0100-0x01FF is left for the cartridge header so the boot ROM can
read it during its handoff).
-}
addrInBootRange :: Bool -> Word16 -> Bool
addrInBootRange cgb addr
    | addr <= 0x00FF = True
    | cgb && addr >= 0x0200 && addr <= 0x08FF = True
    | otherwise = False

{- | @0xFF50@: writing any non-zero value latches the boot ROM off
permanently. Real hardware only inspects bit 0; we follow that.
-}
writeBootRomLock :: Word8 -> Bus -> IO ()
writeBootRomLock v b = when (testBit v 0) (writeIORef (busBootRomActive b) False)

----------------------------------------------------------------------
-- DMG-on-CGB compatibility palette
----------------------------------------------------------------------

{- | Pre-load CGB BG palette 0 and OBJ palettes 0\/1 with a compatibility
auto-palette derived from the cartridge title. The CGB boot ROM does
this for unmodified DMG cartridges based on a title hash; we use a
simple grayscale default for now (a follow-up can add the real
title-hash table for famous titles).
-}
applyCompatPalette :: Header.Header -> Ppu.PpuState -> IO ()
applyCompatPalette _hdr ppu = do
    -- Greyscale default: the four DMG shades map to white, light gray,
    -- dark gray, and near-black. Each color is RGB555 little-endian.
    let bgColors = grayscaleAuto
        obj0 = grayscaleAuto
        obj1 = grayscaleAuto
    writePalEntry (Ppu.ppuBgPalRam ppu) 0 bgColors
    writePalEntry (Ppu.ppuObjPalRam ppu) 0 obj0
    writePalEntry (Ppu.ppuObjPalRam ppu) 1 obj1

{- | Four RGB555 colors (8 bytes total, little-endian) approximating the
CGB boot ROM's "no-title-match" greyscale palette.
-}
grayscaleAuto :: [Word8]
grayscaleAuto =
    [ 0xFF
    , 0x7F -- shade 0: white  (R=31, G=31, B=31)
    , 0x52
    , 0x4A -- shade 1: light gray
    , 0xA9
    , 0x29 -- shade 2: dark gray
    , 0x00
    , 0x00 -- shade 3: black
    ]

writePalEntry :: MV.IOVector Word8 -> Int -> [Word8] -> IO ()
writePalEntry pal palIdx bytes =
    mapM_
        (\(i, b) -> MV.write pal (palIdx * 8 + i) b)
        (zip [0 ..] (take 8 bytes))

{- | Called by the @STOP@ instruction. On a CGB cart with KEY1 bit 0
set, this toggles the double-speed bit and clears the prepare-switch
latch; otherwise it's a no-op (the caller still sets cpuHalted).
-}
triggerSpeedSwitch :: Bus -> IO Bool
triggerSpeedSwitch b
    | not (busCgb b) = pure False
    | otherwise = do
        prep <- readIORef (busKey1 b)
        if testBit prep 0
            then do
                writeIORef (busKey1 b) 0
                writeIORef (busDoubleSpeedAcc b) 0
                modifyIORef' (busDoubleSpeed b) not
                pure True
            else pure False

----------------------------------------------------------------------
-- HDMA (CGB)
----------------------------------------------------------------------

{- | HDMA5 read: bit 7 = 0 while an HBlank-mode transfer is still
running, 1 otherwise; low 7 bits = (remaining-bytes \/ 16) - 1, or
@0x7F@ when idle.
-}
readHdma5 :: Bus -> IO Word8
readHdma5 b
    | not (busCgb b) = pure 0xFF
    | otherwise = do
        active <- readIORef (busHdmaActive b)
        len <- readIORef (busHdmaLen b)
        if active
            then pure (fromIntegral ((len `div` 16) - 1) .&. 0x7F)
            else pure 0xFF

writeHdmaReg :: Word16 -> Word8 -> Bus -> IO ()
writeHdmaReg addr v b = when (busCgb b) $ case addr of
    0xFF51 -> do
        cur <- readIORef (busHdmaSrc b)
        writeIORef (busHdmaSrc b) ((fromIntegral v `shiftL` 8) .|. (cur .&. 0x00FF))
    0xFF52 -> do
        cur <- readIORef (busHdmaSrc b)
        writeIORef (busHdmaSrc b) ((cur .&. 0xFF00) .|. fromIntegral (v .&. 0xF0))
    0xFF53 -> do
        cur <- readIORef (busHdmaDst b)
        let !hi = (fromIntegral (v .&. 0x1F) :: Word16) `shiftL` 8
        writeIORef (busHdmaDst b) (0x8000 .|. hi .|. (cur .&. 0x00FF))
    0xFF54 -> do
        cur <- readIORef (busHdmaDst b)
        writeIORef (busHdmaDst b) ((cur .&. 0xFF00) .|. fromIntegral (v .&. 0xF0))
    0xFF55 -> startOrStopHdma v b
    _ -> pure ()

{- | Handle a write to HDMA5. Three cases:

* HBlank DMA already active and bit 7 is 0: stop the transfer.
* Bit 7 is 1: start (or restart) an HBlank-mode transfer of
  @((v & 0x7F) + 1) * 16@ bytes; chunks are copied later from
  'advance' on each HBlank entry.
* Bit 7 is 0 with no active HBlank transfer: copy the full payload
  immediately (general-mode DMA).
-}
startOrStopHdma :: Word8 -> Bus -> IO ()
startOrStopHdma v b = do
    active <- readIORef (busHdmaActive b)
    let !lenBytes = (fromIntegral (v .&. 0x7F) + 1) * 16
        !hblank = testBit v 7
    if active && not hblank
        then writeIORef (busHdmaActive b) False
        else do
            writeIORef (busHdmaLen b) lenBytes
            if hblank
                then writeIORef (busHdmaActive b) True
                else do
                    writeIORef (busHdmaActive b) False
                    runGeneralHdma b

-- | Drain the entire HDMA payload immediately (general-mode transfer).
runGeneralHdma :: Bus -> IO ()
runGeneralHdma b = do
    len <- readIORef (busHdmaLen b)
    src <- readIORef (busHdmaSrc b)
    dst <- readIORef (busHdmaDst b)
    copyHdmaBytes b src dst len
    advanceHdmaPointers b len
    writeIORef (busHdmaLen b) 0
    -- General DMA stalls the CPU for the duration of the copy: 8 M-cycles
    -- per 16 bytes in single-speed (16 M-cycles per 16 bytes in double-
    -- speed). Advance the peripherals so they continue to tick during the
    -- block instead of jumping forward only when the next instruction runs.
    ds <- readIORef (busDoubleSpeed b)
    let !blockCycles = if ds then len else len `div` 2
    advance blockCycles b

{- | Copy one 16-byte chunk for an active HBlank-mode transfer; called
by 'advance' when the PPU enters Mode 0. Marks the transfer
inactive once the last chunk lands.
-}
stepHdmaHBlank :: Bus -> IO ()
stepHdmaHBlank b = do
    active <- readIORef (busHdmaActive b)
    when active $ do
        len <- readIORef (busHdmaLen b)
        when (len > 0) $ do
            src <- readIORef (busHdmaSrc b)
            dst <- readIORef (busHdmaDst b)
            copyHdmaBytes b src dst 16
            advanceHdmaPointers b 16
            let !len' = len - 16
            writeIORef (busHdmaLen b) len'
            when (len' == 0) (writeIORef (busHdmaActive b) False)

copyHdmaBytes :: Bus -> Word16 -> Word16 -> Int -> IO ()
copyHdmaBytes b src dst n =
    mapM_
        ( \i -> do
            byte <- read8 (src + fromIntegral i) b
            -- Direct VRAM write (respects current VBK) bypassing the
            -- bus dispatcher to avoid recursion.
            Ppu.write8 (dst + fromIntegral i) byte (busPpu b)
        )
        [0 .. n - 1]

advanceHdmaPointers :: Bus -> Int -> IO ()
advanceHdmaPointers b n = do
    modifyIORef' (busHdmaSrc b) (+ fromIntegral n)
    modifyIORef' (busHdmaDst b) (+ fromIntegral n)

{- | OAM DMA: copy 160 bytes from @(v << 8)@ into OAM, going through the bus
read path so any source region (cart ROM/RAM, WRAM) works. Done instantly;
the real 160-cycle delay and CPU lockout are not modeled.
-}
{- | Schedule an OAM DMA. The transfer doesn't copy any bytes during the
M-cycles of the instruction that triggered it: 'busOamDmaStarting' is
held high through the rest of the current 'advance' window. The first
byte copy lands on the first M-cycle of the *next* CPU instruction,
modeling the documented "DMA starts after the cycle in which it was
triggered" behavior.
-}
oamDma :: Word8 -> Bus -> IO ()
oamDma srcHi b = do
    -- Latch the source byte so reads of FF46 return the value last
    -- written (mooneye oam_dma/reg_read). The latch happens immediately
    -- and is unaffected by DMA being already in progress (a second write
    -- restarts the transfer per oam_dma_restart).
    MV.write (busIo b) 0x46 srcHi
    let srcAddr = (fromIntegral srcHi :: Word16) `shiftL` 8
    writeIORef (busOamDmaSrc b) srcAddr
    writeIORef (busOamDmaIndex b) 0
    writeIORef (busOamDmaActive b) True
    writeIORef (busOamDmaStarting b) True

{- | Step OAM DMA by one M-cycle. Called once per peripheral M-cycle in
'advance'. The first cycle after a FF46 write is consumed by the
"starting" delay; subsequent cycles each copy one byte from
@src + index@ to OAM, advancing the index. Reaching index 160
deactivates the transfer.

Reads from VRAM during the PPU's mode 3 normally return @0xFF@; we read
through 'read8' which already returns the locked value, so a DMA whose
source overlaps VRAM produces the same garbled OAM as on hardware.
-}
stepOamDma :: Bus -> IO ()
stepOamDma b = do
    active <- readIORef (busOamDmaActive b)
    starting <- readIORef (busOamDmaStarting b)
    when (active && not starting) $ do
        idx <- readIORef (busOamDmaIndex b)
        src <- readIORef (busOamDmaSrc b)
        -- Read directly from the underlying memory rather than through
        -- 'read8', so the DMA itself isn't subject to the lockout it
        -- imposes on the CPU.
        byte <- readDmaSource (src + fromIntegral idx) b
        MV.write (Ppu.ppuOam (busPpu b)) idx byte
        let idx' = idx + 1
        writeIORef (busOamDmaIndex b) idx'
        when (idx' >= 160) (writeIORef (busOamDmaActive b) False)

{- | DMA-internal source read. Bypasses the 'busOamDmaActive' lockout that
'read8' applies to CPU accesses; otherwise routes the same way. (DMA is
the one bus master that can still see memory while it's running.)
-}
readDmaSource :: Word16 -> Bus -> IO Word8
readDmaSource addr b
    | addr <= 0x7FFF = bootRomOrCart addr b
    | addr <= 0x9FFF = Ppu.read8 addr (busPpu b)
    | addr <= 0xBFFF = Cartridge.read8 addr (busCart b)
    | addr <= 0xCFFF = MV.read (busWram b) (fromIntegral (addr - 0xC000))
    | addr <= 0xDFFF = readUpperWram (addr - 0xD000) b
    | addr <= 0xFDFF = readEcho (addr - 0xE000) b
    -- Some games trigger DMA from FExx: real hardware mirrors the lower
    -- 8 KiB of WRAM up through 0xFEFF for DMA reads. The CPU view of
    -- 0xFEA0-0xFEFF is "unusable" but DMA still gets a byte.
    | addr <= 0xFEFF = MV.read (busWram b) (fromIntegral (addr .&. 0x1FFF))
    | otherwise = pure 0xFF

{- | Advance time-driven subsystems by N M-cycles. Ticks Timer and PPU and
latches the Timer interrupt (bit 2) and VBlank (bit 0) into @IF@ at @0xFF0F@.
-}
advance :: Int -> Bus -> IO ()
advance mCycles b = do
    -- In double-speed mode the CPU clock is twice as fast, so the
    -- peripherals (timer, PPU, APU) should see half as many M-cycles
    -- per CPU instruction. Track odd cycles in a 0/1 accumulator.
    ds <- readIORef (busDoubleSpeed b)
    pCycles <-
        if ds
            then do
                acc <- readIORef (busDoubleSpeedAcc b)
                let total = acc + mCycles
                writeIORef (busDoubleSpeedAcc b) (total `mod` 2)
                pure (total `div` 2)
            else pure mCycles
    ts <- readIORef (busTimer b)
    let (ts', overflow) = Timer.advance pCycles ts
    writeIORef (busTimer b) ts'
    ppuIrqs <- Ppu.advance pCycles (busPpu b)
    Apu.advance pCycles (busApu b)
    -- OAM DMA copies one byte per peripheral M-cycle. Stepping it after
    -- the PPU advance means the DMA reads OAM/VRAM at the freshly-updated
    -- mode timing.
    replicateM_ pCycles (stepOamDma b)
    -- The "starting" flag holds the DMA off for the duration of the
    -- triggering instruction (we run advance after the instruction has
    -- already completed its register-store side-effect). Clearing it at
    -- the end of advance lets copying begin on the *next* instruction's
    -- first M-cycle, matching the documented 1-cycle startup delay.
    writeIORef (busOamDmaStarting b) False
    when overflow (setIfBit 2 b) -- Timer
    when (testBit ppuIrqs 0) (setIfBit 0 b) -- VBlank
    when (testBit ppuIrqs 1) (setIfBit 1 b) -- LCD STAT
    -- HBlank-entered signal (bit 2): step one HDMA chunk, not an interrupt.
    when (testBit ppuIrqs 2) (stepHdmaHBlank b)
    -- Joypad IRQ pending edge (set by Joypad.setButton).
    jpEdge <- Joypad.takeIrqPending (busJoypad b)
    when jpEdge (setIfBit 4 b)

-- | Drain the APU's pending stereo samples (interleaved L,R) for the frontend.
drainAudioSamples :: Bus -> IO [Int16]
drainAudioSamples b = Apu.drainSamples (busApu b)

setIfBit :: Int -> Bus -> IO ()
setIfBit n b = do
    iflag <- MV.read (busIo b) 0x0F
    MV.write (busIo b) 0x0F (setBit iflag n)

drainSerial :: Bus -> IO [Word8]
drainSerial b = reverse <$> readIORef (busSerialOut b)
