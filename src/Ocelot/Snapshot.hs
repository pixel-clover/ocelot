{- | Save / load full machine snapshots.

A snapshot freezes the entire CPU + Bus + cartridge bank-state + PPU +
APU + Timer + Joypad into a flat byte string suitable for stashing in
memory or writing to a @.state@ file. Cartridge ROM is /not/ written;
loading requires the same ROM that was running at snapshot time.

Format (all little-endian):

> magic "OCS1"      4 bytes
> version           u32   (currently 1)
> CPU section       fixed 24 bytes
> Timer section     fixed 7 bytes
> Joypad section    fixed 3 bytes
> PPU regs section  fixed 16 bytes (11 byte regs + 1 mode + 4 dot)
> PPU VRAM/OAM/FB   3x length-prefixed blobs
> APU blob          1x length-prefixed
> Bus WRAM/HRAM/IO  3x length-prefixed blobs
> Bus IE            u8
> Cart RAM+RTC blob 1x length-prefixed (output of 'extractSave')
> Cart MBC blob     1x length-prefixed (output of 'dumpMbc')

Sections are framed with length prefixes only where the payload is
variable-size; the fixed-size ones are inlined directly to keep the
format compact.
-}
module Ocelot.Snapshot (
    SnapshotError (..),
    save,
    load,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word32, Word8)
import qualified Ocelot.Apu as Apu
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cart
import Ocelot.Cpu.Registers (Registers (..))
import Ocelot.Cpu.State (CpuState (..))
import qualified Ocelot.Joypad as Joypad
import Ocelot.Machine (Machine (..))
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot.Binary as Snap
import Ocelot.Timer (TimerState (..))

data SnapshotError
    = BadMagic
    | UnsupportedVersion !Word32
    | TruncatedBlob
    deriving (Eq, Show)

magic :: ByteString
magic = BS.pack [0x4F, 0x43, 0x53, 0x31] -- "OCS1"

currentVersion :: Word32
currentVersion = 1

cpuLen, timerLen, joyLen, ppuRegLen :: Int
cpuLen = 24
timerLen = 7
joyLen = 3
ppuRegLen = 16

----------------------------------------------------------------------
-- Save
----------------------------------------------------------------------

save :: Machine -> IO ByteString
save m = do
    let bus = machineBus m
    cpu <- readIORef (machineCpu m)
    timer <- readIORef (Bus.busTimer bus)
    ppuBytes <- ppuSnapshot (Bus.busPpu bus)
    apuBlob <- Apu.dumpState (Bus.busApu bus)
    busBytes <- busSnapshot bus
    jp <- Joypad.dumpState (Bus.busJoypad bus)
    cartRamBlob <- Cart.extractSave (Bus.busCart bus)
    cartMbcBlob <- Cart.dumpMbc (Bus.busCart bus)
    let bb =
            BB.byteString magic
                <> Snap.putU32 currentVersion
                <> encodeCpu cpu
                <> encodeTimer timer
                <> encodeJoypad jp
                <> ppuBytes
                <> Snap.putBlob apuBlob
                <> busBytes
                <> Snap.putBlob cartRamBlob
                <> Snap.putBlob cartMbcBlob
    pure (BL.toStrict (BB.toLazyByteString bb))

encodeCpu :: CpuState -> BB.Builder
encodeCpu c =
    let r = cpuRegs c
     in Snap.putU8 (regA r)
            <> Snap.putU8 (regF r)
            <> Snap.putU8 (regB r)
            <> Snap.putU8 (regC r)
            <> Snap.putU8 (regD r)
            <> Snap.putU8 (regE r)
            <> Snap.putU8 (regH r)
            <> Snap.putU8 (regL r)
            <> Snap.putU16 (regSP r)
            <> Snap.putU16 (regPC r)
            <> Snap.putBool (cpuIme c)
            <> Snap.putBool (cpuEiDelay c)
            <> Snap.putBool (cpuHalted c)
            <> Snap.putU8 0 -- pad
            <> Snap.putI64 (fromIntegral (cpuCycles c))

encodeTimer :: TimerState -> BB.Builder
encodeTimer ts =
    Snap.putU16 (timDivider ts)
        <> Snap.putU16 (timTimaAccum ts)
        <> Snap.putU8 (timTima ts)
        <> Snap.putU8 (timTma ts)
        <> Snap.putU8 (timTac ts)

encodeJoypad :: (Word8, Word8, Bool) -> BB.Builder
encodeJoypad (sel, mask, irq) =
    Snap.putU8 sel <> Snap.putU8 mask <> Snap.putBool irq

ppuSnapshot :: Ppu.PpuState -> IO BB.Builder
ppuSnapshot ps = do
    lcdc <- readIORef (Ppu.ppuLcdc ps)
    stat <- readIORef (Ppu.ppuStat ps)
    ly <- readIORef (Ppu.ppuLy ps)
    lyc <- readIORef (Ppu.ppuLyc ps)
    scy <- readIORef (Ppu.ppuScy ps)
    scx <- readIORef (Ppu.ppuScx ps)
    wy <- readIORef (Ppu.ppuWy ps)
    wx <- readIORef (Ppu.ppuWx ps)
    bgp <- readIORef (Ppu.ppuBgp ps)
    obp0 <- readIORef (Ppu.ppuObp0 ps)
    obp1 <- readIORef (Ppu.ppuObp1 ps)
    mode <- readIORef (Ppu.ppuMode ps)
    dot <- readIORef (Ppu.ppuDot ps)
    vram <- ioVectorBytes (Ppu.ppuVram ps)
    oam <- ioVectorBytes (Ppu.ppuOam ps)
    fb <- ioVectorBytes (Ppu.ppuFb ps)
    pure $
        Snap.putU8 lcdc
            <> Snap.putU8 stat
            <> Snap.putU8 ly
            <> Snap.putU8 lyc
            <> Snap.putU8 scy
            <> Snap.putU8 scx
            <> Snap.putU8 wy
            <> Snap.putU8 wx
            <> Snap.putU8 bgp
            <> Snap.putU8 obp0
            <> Snap.putU8 obp1
            <> Snap.putU8 (fromIntegral (fromEnum mode))
            <> Snap.putU32 (fromIntegral dot)
            <> Snap.putBlob vram
            <> Snap.putBlob oam
            <> Snap.putBlob fb

busSnapshot :: Bus.Bus -> IO BB.Builder
busSnapshot b = do
    wram <- ioVectorBytes (Bus.busWram b)
    hram <- ioVectorBytes (Bus.busHram b)
    io <- ioVectorBytes (Bus.busIo b)
    ie <- readIORef (Bus.busIe b)
    pure $
        Snap.putBlob wram
            <> Snap.putBlob hram
            <> Snap.putBlob io
            <> Snap.putU8 ie

ioVectorBytes :: MV.IOVector Word8 -> IO ByteString
ioVectorBytes v = do
    frozen <- V.freeze v
    pure (BS.pack (V.toList frozen))

----------------------------------------------------------------------
-- Load
----------------------------------------------------------------------

load :: ByteString -> Machine -> IO (Either SnapshotError ())
load bs m
    | BS.length bs < 8 = pure (Left TruncatedBlob)
    | BS.take 4 bs /= magic = pure (Left BadMagic)
    | otherwise =
        let ver = decodeU32 (BS.drop 4 bs)
         in if ver /= currentVersion
                then pure (Left (UnsupportedVersion ver))
                else do
                    applySnapshot (BS.drop 8 bs) m
                    pure (Right ())

applySnapshot :: ByteString -> Machine -> IO ()
applySnapshot bs0 m = do
    let bus = machineBus m
    applyCpu (BS.take cpuLen bs0) m
    let bs1 = BS.drop cpuLen bs0
    applyTimer (BS.take timerLen bs1) bus
    let bs2 = BS.drop timerLen bs1
    applyJoypad (BS.take joyLen bs2) bus
    let bs3 = BS.drop joyLen bs2
    applyPpuRegs (BS.take ppuRegLen bs3) (Bus.busPpu bus)
    let bs4 = BS.drop ppuRegLen bs3
        (vram, bs5) = takeBlob bs4
        (oam, bs6) = takeBlob bs5
        (fb, bs7) = takeBlob bs6
    writeBytesToVector vram (Ppu.ppuVram (Bus.busPpu bus))
    writeBytesToVector oam (Ppu.ppuOam (Bus.busPpu bus))
    writeBytesToVector fb (Ppu.ppuFb (Bus.busPpu bus))
    let (apuBlob, bs8) = takeBlob bs7
    Apu.loadState apuBlob (Bus.busApu bus)
    let (wram, bs9) = takeBlob bs8
        (hram, bs10) = takeBlob bs9
        (io, bs11) = takeBlob bs10
    writeBytesToVector wram (Bus.busWram bus)
    writeBytesToVector hram (Bus.busHram bus)
    writeBytesToVector io (Bus.busIo bus)
    if BS.null bs11
        then pure ()
        else writeIORef (Bus.busIe bus) (BS.index bs11 0)
    let bs12 = BS.drop 1 bs11
        (cartRam, bs13) = takeBlob bs12
        (cartMbc, _) = takeBlob bs13
    Cart.loadSave cartRam (Bus.busCart bus)
    Cart.loadMbc cartMbc (Bus.busCart bus)

takeBlob :: ByteString -> (ByteString, ByteString)
takeBlob bs
    | BS.length bs < 4 = (BS.empty, BS.empty)
    | otherwise =
        let n = fromIntegral (decodeU32 bs)
            payload = BS.take n (BS.drop 4 bs)
            rest = BS.drop (4 + n) bs
         in (payload, rest)

writeBytesToVector :: ByteString -> MV.IOVector Word8 -> IO ()
writeBytesToVector bs v = do
    let n = min (BS.length bs) (MV.length v)
    mapM_ (\i -> MV.write v i (BS.index bs i)) [0 .. n - 1]

applyCpu :: ByteString -> Machine -> IO ()
applyCpu bs m = do
    let r =
            Registers
                { regA = BS.index bs 0
                , regF = BS.index bs 1
                , regB = BS.index bs 2
                , regC = BS.index bs 3
                , regD = BS.index bs 4
                , regE = BS.index bs 5
                , regH = BS.index bs 6
                , regL = BS.index bs 7
                , regSP = decodeU16 (BS.drop 8 bs)
                , regPC = decodeU16 (BS.drop 10 bs)
                }
        ime = BS.index bs 12 /= 0
        ei = BS.index bs 13 /= 0
        halted = BS.index bs 14 /= 0
        cycles = fromIntegral (decodeI64 (BS.drop 16 bs))
    writeIORef
        (machineCpu m)
        CpuState
            { cpuRegs = r
            , cpuIme = ime
            , cpuEiDelay = ei
            , cpuHalted = halted
            , cpuCycles = cycles
            }

applyTimer :: ByteString -> Bus.Bus -> IO ()
applyTimer bs bus =
    writeIORef
        (Bus.busTimer bus)
        TimerState
            { timDivider = decodeU16 bs
            , timTimaAccum = decodeU16 (BS.drop 2 bs)
            , timTima = BS.index bs 4
            , timTma = BS.index bs 5
            , timTac = BS.index bs 6
            }

applyJoypad :: ByteString -> Bus.Bus -> IO ()
applyJoypad bs bus =
    Joypad.loadState
        ( BS.index bs 0
        , BS.index bs 1
        , BS.index bs 2 /= 0
        )
        (Bus.busJoypad bus)

applyPpuRegs :: ByteString -> Ppu.PpuState -> IO ()
applyPpuRegs bs ps = do
    writeIORef (Ppu.ppuLcdc ps) (BS.index bs 0)
    writeIORef (Ppu.ppuStat ps) (BS.index bs 1)
    writeIORef (Ppu.ppuLy ps) (BS.index bs 2)
    writeIORef (Ppu.ppuLyc ps) (BS.index bs 3)
    writeIORef (Ppu.ppuScy ps) (BS.index bs 4)
    writeIORef (Ppu.ppuScx ps) (BS.index bs 5)
    writeIORef (Ppu.ppuWy ps) (BS.index bs 6)
    writeIORef (Ppu.ppuWx ps) (BS.index bs 7)
    writeIORef (Ppu.ppuBgp ps) (BS.index bs 8)
    writeIORef (Ppu.ppuObp0 ps) (BS.index bs 9)
    writeIORef (Ppu.ppuObp1 ps) (BS.index bs 10)
    let modeByte = BS.index bs 11
    writeIORef (Ppu.ppuMode ps) (toEnum (fromIntegral modeByte))
    writeIORef (Ppu.ppuDot ps) (fromIntegral (decodeU32 (BS.drop 12 bs)))

decodeU16 :: ByteString -> Word16
decodeU16 bs =
    fromIntegral (BS.index bs 0)
        .|. (fromIntegral (BS.index bs 1) `shiftL` 8)

decodeU32 :: ByteString -> Word32
decodeU32 bs =
    fromIntegral (BS.index bs 0)
        .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
        .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
        .|. (fromIntegral (BS.index bs 3) `shiftL` 24)

decodeI64 :: ByteString -> Int
decodeI64 bs =
    let b i = fromIntegral (BS.index bs i) :: Int
     in b 0
            .|. (b 1 `shiftL` 8)
            .|. (b 2 `shiftL` 16)
            .|. (b 3 `shiftL` 24)
            .|. (b 4 `shiftL` 32)
            .|. (b 5 `shiftL` 40)
            .|. (b 6 `shiftL` 48)
            .|. (b 7 `shiftL` 56)
