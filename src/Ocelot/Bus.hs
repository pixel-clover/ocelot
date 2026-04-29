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
) where

import Control.Monad (when)
import Data.Bits (setBit, shiftL, testBit, (.&.), (.|.))
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
    -- switch" (writable); bit 7 = current speed (always 0 for now —
    -- double-speed timing is not yet implemented).
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
    let cgb = case Header.hdrCgbFlag (Cartridge.cartridgeHeader c) of
            Header.DmgOnly -> False
            Header.DmgAndCgb -> True
            Header.CgbOnly -> True
    Ppu.setCgbMode cgb ppu
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
            }

read8 :: Word16 -> Bus -> IO Word8
read8 addr b
    | addr <= 0x7FFF = Cartridge.read8 addr (busCart b)
    | addr <= 0x9FFF = Ppu.read8 addr (busPpu b)
    | addr <= 0xBFFF = Cartridge.read8 addr (busCart b)
    | addr <= 0xCFFF = MV.read (busWram b) (fromIntegral (addr - 0xC000))
    | addr <= 0xDFFF = readUpperWram (addr - 0xD000) b
    | addr <= 0xFDFF = readEcho (addr - 0xE000) b
    | addr <= 0xFE9F = Ppu.read8 addr (busPpu b)
    | addr <= 0xFEFF = pure 0xFF
    | addr == 0xFF00 = Joypad.readP1 (busJoypad b)
    | addr == 0xFF04 = Timer.readDiv <$> readIORef (busTimer b)
    | addr == 0xFF05 = Timer.readTima <$> readIORef (busTimer b)
    | addr == 0xFF06 = Timer.readTma <$> readIORef (busTimer b)
    | addr == 0xFF07 = Timer.readTac <$> readIORef (busTimer b)
    | addr >= 0xFF10 && addr <= 0xFF26 = Apu.read8 addr (busApu b)
    | addr >= 0xFF30 && addr <= 0xFF3F = Apu.read8 addr (busApu b)
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.read8 addr (busPpu b)
    | addr == 0xFF4D = readKey1 b
    | addr == 0xFF4F = Ppu.read8 addr (busPpu b)
    | addr == 0xFF68 = Ppu.read8 addr (busPpu b)
    | addr == 0xFF69 = Ppu.read8 addr (busPpu b)
    | addr == 0xFF6A = Ppu.read8 addr (busPpu b)
    | addr == 0xFF6B = Ppu.read8 addr (busPpu b)
    | addr == 0xFF70 = readWramBank b
    | addr <= 0xFF7F = MV.read (busIo b) (fromIntegral (addr - 0xFF00))
    | addr <= 0xFFFE = MV.read (busHram b) (fromIntegral (addr - 0xFF80))
    | otherwise = readIORef (busIe b)

write8 :: Word16 -> Word8 -> Bus -> IO ()
write8 addr !v b
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
    | addr >= 0xFF10 && addr <= 0xFF26 = Apu.write8 addr v (busApu b)
    | addr >= 0xFF30 && addr <= 0xFF3F = Apu.write8 addr v (busApu b)
    | addr == 0xFF46 = oamDma v b
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.write8 addr v (busPpu b)
    | addr == 0xFF4D = writeKey1 v b
    | addr == 0xFF4F = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF68 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF69 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6A = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6B = when (busCgb b) (Ppu.write8 addr v (busPpu b))
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
    | otherwise = (\x -> x .|. 0xF8) <$> readIORef (busWramBank b)

writeWramBank :: Word8 -> Bus -> IO ()
writeWramBank v b = when (busCgb b) (writeIORef (busWramBank b) (v .&. 0x07))

{- | KEY1 read: bit 7 = current speed (always 0 since double-speed isn't
modeled yet), bit 0 = pending switch. Bits 1..6 read as 1.
-}
readKey1 :: Bus -> IO Word8
readKey1 b
    | not (busCgb b) = pure 0xFF
    | otherwise = (\x -> x .|. 0x7E) <$> readIORef (busKey1 b)

writeKey1 :: Word8 -> Bus -> IO ()
writeKey1 v b = when (busCgb b) (writeIORef (busKey1 b) (v .&. 0x01))

{- | OAM DMA: copy 160 bytes from @(v << 8)@ into OAM, going through the bus
read path so any source region (cart ROM/RAM, WRAM) works. Done instantly;
the real 160-cycle delay and CPU lockout are not modeled.
-}
oamDma :: Word8 -> Bus -> IO ()
oamDma srcHi b = do
    let srcAddr = (fromIntegral srcHi :: Word16) `shiftL` 8
    mapM_
        ( \i -> do
            byte <- read8 (srcAddr + fromIntegral i) b
            MV.write (Ppu.ppuOam (busPpu b)) i byte
        )
        [0 .. 0x9F :: Int]

{- | Advance time-driven subsystems by N M-cycles. Ticks Timer and PPU and
latches the Timer interrupt (bit 2) and VBlank (bit 0) into @IF@ at @0xFF0F@.
-}
advance :: Int -> Bus -> IO ()
advance mCycles b = do
    ts <- readIORef (busTimer b)
    let (ts', overflow) = Timer.advance mCycles ts
    writeIORef (busTimer b) ts'
    ppuIrqs <- Ppu.advance mCycles (busPpu b)
    Apu.advance mCycles (busApu b)
    when overflow (setIfBit 2 b) -- Timer
    when (testBit ppuIrqs 0) (setIfBit 0 b) -- VBlank
    when (testBit ppuIrqs 1) (setIfBit 1 b) -- LCD STAT
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
