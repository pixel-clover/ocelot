{- | Save / load full machine snapshots.

A snapshot freezes the entire CPU + Bus + cartridge bank-state + PPU +
APU + Timer + Joypad into a flat byte string suitable for stashing in
memory or writing to a @.state@ file. Cartridge ROM is /not/ written;
loading requires the same ROM that was running at snapshot time.

Format (all little-endian):

> magic "OCS1"      4 bytes
> version           u32   (see 'currentVersion')
> CPU section       fixed 24 bytes
> Timer section     fixed 8 bytes
> Joypad section    fixed 3 bytes
> PPU regs section  fixed 16 bytes (11 byte regs + 1 mode + 4 dot)
> PPU VRAM/OAM/FB   3x length-prefixed blobs
> PPU CGB block     u8 vbk + u8 bcps + u8 ocps + 2x palette blobs
> PPU window line   u32
> PPU STAT edge     u8 prev-line + u8 pending-irq
> PPU OPRI          u8
> APU blob          1x length-prefixed
> Bus WRAM/HRAM/IO  3x length-prefixed blobs
> Bus IE            u8
> Bus CGB block     u8 wbk + u8 key1
> Bus HDMA block    u16 src + u16 dst + u32 len + 3x bool
> Bus OAM DMA       u8 active + u8 starting + u16 src + u8 index
> Cart RAM+RTC blob 1x length-prefixed (output of 'extractSave')
> Cart MBC blob     1x length-prefixed (output of 'dumpMbc')

Sections are framed with length prefixes only where the payload is
variable-size; the fixed-size ones are inlined directly to keep the
format compact.

Loading is all-or-nothing: the whole blob is decoded into a pure
'SnapshotData' through a bounds-checked cursor first, and only a
complete decode is written into the live machine. A short or corrupt
blob returns 'TruncatedBlob' with the machine untouched.
-}
module Ocelot.Snapshot (
    SnapshotError (..),
    currentVersion,
    save,
    load,
) where

import Data.Bits ((.&.))
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

{- | Blob format version.

Version 1 blobs are rejected with 'UnsupportedVersion': the format had in fact
changed several times under that number, so a v1 blob's section layout is not
knowable and accepting it would half-restore into garbage. Version 2 was the
loader becoming strict plus the APU CH1 sweep negate-used latch.

The constant then sat at 2 while the section layout kept growing, so the @v3:@
through @v8:@ labels on the sections below never had a bump behind them and no
blob was ever written with those numbers. Version 9 adds the PPU
short-first-line-after-LCD-on latch and realigns the constant with those labels
in one step. Any blob on disk predating this is a 2 and is rejected, which is
correct: its PPU section is a byte shorter.

Keep this history current. A section change without a bump is what produced the
gap in the first place.
-}
currentVersion :: Word32
currentVersion = 9

----------------------------------------------------------------------
-- Save
----------------------------------------------------------------------

save :: Machine -> IO ByteString
save m = do
    let bus = machineBus m
    -- The bus defers APU stepping (see 'Ocelot.Bus.flushApu'), and
    -- 'Apu.dumpState' reaches the APU directly rather than through the bus,
    -- so settle the debt first or the snapshot captures a stale APU. This is
    -- semantically a no-op: the debt would have been settled at the next
    -- register touch anyway, and it is zeroed here rather than dropped.
    Bus.flushApu bus
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
        <> Snap.putU8 (timTima ts)
        <> Snap.putU8 (timTma ts)
        <> Snap.putU8 (timTac ts)
        <> Snap.putBool (timPrevAnd ts)
        <> Snap.putU8 (fromIntegral (timReloadCounter ts))
        <> Snap.putU8 (fromIntegral (timReloadedCounter ts))

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
    -- CGB additions (v2): VBK, BCPS, OCPS, BG palette RAM, OBJ palette RAM.
    vbk <- readIORef (Ppu.ppuVbk ps)
    bcps <- readIORef (Ppu.ppuBcps ps)
    ocps <- readIORef (Ppu.ppuOcps ps)
    bgPal <- ioVectorBytes (Ppu.ppuBgPalRam ps)
    objPal <- ioVectorBytes (Ppu.ppuObjPalRam ps)
    -- v4 addition: window-line counter.
    wly <- readIORef (Ppu.ppuWindowLine ps)
    -- v7 addition: STAT edge-detector latches. Without these, restoring
    -- a snapshot that was taken with a high STAT line would resume with
    -- prev=False and immediately re-fire the edge on the next mode
    -- transition; restoring one with a pending IRQ that hadn't yet been
    -- consumed by the bus would lose the IRQ.
    prevStat <- readIORef (Ppu.ppuPrevStatLine ps)
    pendStat <- readIORef (Ppu.ppuPendingStatIrq ps)
    -- v8 addition: OPRI (0xFF6C). CGB-compat carts that boot with
    -- OPRI=1 would resume with the default 0 if not snapshotted, which
    -- silently flips sprite Z-ordering on reload.
    opri <- readIORef (Ppu.ppuOpri ps)
    -- v9 addition: the short-first-line latch. A snapshot taken during the
    -- first (448-dot) scanline after the LCD was enabled would otherwise resume
    -- on a full 456-dot line and land the rest of the frame 8 T-cycles late.
    lcdOnFirst <- readIORef (Ppu.ppuLcdOnFirstLine ps)
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
            -- CGB block (v2):
            <> Snap.putU8 vbk
            <> Snap.putU8 bcps
            <> Snap.putU8 ocps
            <> Snap.putBlob bgPal
            <> Snap.putBlob objPal
            -- v4: window-line counter (u32 to leave room for tall frames).
            <> Snap.putU32 (fromIntegral wly)
            -- v7: STAT edge-detector latches.
            <> Snap.putBool prevStat
            <> Snap.putBool pendStat
            -- v8: OPRI register.
            <> Snap.putU8 opri
            -- v9: short-first-line-after-LCD-on latch.
            <> Snap.putBool lcdOnFirst

busSnapshot :: Bus.Bus -> IO BB.Builder
busSnapshot b = do
    wram <- ioVectorBytes (Bus.busWram b)
    hram <- ioVectorBytes (Bus.busHram b)
    io <- ioVectorBytes (Bus.busIo b)
    ie <- readIORef (Bus.busIe b)
    -- CGB additions (v2): WRAM bank selector, KEY1.
    wbk <- readIORef (Bus.busWramBank b)
    key1 <- readIORef (Bus.busKey1 b)
    -- v3 additions: in-flight HDMA + double-speed bits.
    hdmaSrc <- readIORef (Bus.busHdmaSrc b)
    hdmaDst <- readIORef (Bus.busHdmaDst b)
    hdmaLen <- readIORef (Bus.busHdmaLen b)
    hdmaActive <- readIORef (Bus.busHdmaActive b)
    ds <- readIORef (Bus.busDoubleSpeed b)
    dsAcc <- readIORef (Bus.busDoubleSpeedAcc b)
    -- v7 additions: in-flight OAM DMA. Without these, a snapshot taken
    -- while OAM DMA is mid-copy resumes with the DMA dropped, leaving
    -- the partially-copied OAM region in whatever state the snapshot
    -- captured but never finishing the remaining bytes.
    oamActive <- readIORef (Bus.busOamDmaActive b)
    oamStarting <- readIORef (Bus.busOamDmaStarting b)
    oamSrc <- readIORef (Bus.busOamDmaSrc b)
    oamIndex <- readIORef (Bus.busOamDmaIndex b)
    pure $
        Snap.putBlob wram
            <> Snap.putBlob hram
            <> Snap.putBlob io
            <> Snap.putU8 ie
            <> Snap.putU8 wbk
            <> Snap.putU8 key1
            <> Snap.putU16 hdmaSrc
            <> Snap.putU16 hdmaDst
            <> Snap.putU32 (fromIntegral hdmaLen)
            <> Snap.putBool hdmaActive
            <> Snap.putBool ds
            <> Snap.putU8 (fromIntegral dsAcc)
            -- v7: OAM DMA state.
            <> Snap.putBool oamActive
            <> Snap.putBool oamStarting
            <> Snap.putU16 oamSrc
            <> Snap.putU8 (fromIntegral oamIndex)

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
    | ver /= currentVersion = pure (Left (UnsupportedVersion ver))
    | otherwise = case Snap.runCursorChecked decodeSnapshot (BS.drop 8 bs) of
        Nothing -> pure (Left TruncatedBlob)
        Just sd -> do
            applySnapshot sd m
            pure (Right ())
  where
    ver = Snap.runCursor Snap.getU32 (BS.drop 4 bs)

{- | The whole snapshot payload, decoded but not yet installed. Keeping the
decode separate from the apply is what makes a rejected load leave the
machine untouched.
-}
data SnapshotData = SnapshotData
    { sdCpu :: !CpuState
    , sdTimer :: !TimerState
    , sdJoypad :: !(Word8, Word8, Bool)
    , sdPpu :: !PpuData
    , sdApu :: !ByteString
    , sdBus :: !BusData
    , sdCartRam :: !ByteString
    , sdCartMbc :: !ByteString
    }

data PpuData = PpuData
    { pdLcdc, pdStat, pdLy, pdLyc, pdScy, pdScx, pdWy, pdWx :: !Word8
    , pdBgp, pdObp0, pdObp1 :: !Word8
    , pdMode :: !Ppu.PpuMode
    , pdDot :: !Int
    , pdVram, pdOam, pdFb :: !ByteString
    , pdVbk, pdBcps, pdOcps :: !Word8
    , pdBgPal, pdObjPal :: !ByteString
    , pdWindowLine :: !Int
    , pdPrevStat, pdPendingStat :: !Bool
    , pdOpri :: !Word8
    , pdLcdOnFirstLine :: !Bool
    }

data BusData = BusData
    { bdWram, bdHram, bdIo :: !ByteString
    , bdIe, bdWramBank, bdKey1 :: !Word8
    , bdHdmaSrc, bdHdmaDst :: !Word16
    , bdHdmaLen :: !Int
    , bdHdmaActive, bdDoubleSpeed :: !Bool
    , bdDoubleSpeedAcc :: !Int
    , bdOamActive, bdOamStarting :: !Bool
    , bdOamSrc :: !Word16
    , bdOamIndex :: !Int
    }

decodeSnapshot :: Snap.Cursor SnapshotData
decodeSnapshot = do
    cpu <- decodeCpu
    timer <- decodeTimer
    joy <- decodeJoypad
    ppu <- decodePpu
    apu <- Snap.getBlob
    bus <- decodeBus
    cartRam <- Snap.getBlob
    cartMbc <- Snap.getBlob
    pure
        SnapshotData
            { sdCpu = cpu
            , sdTimer = timer
            , sdJoypad = joy
            , sdPpu = ppu
            , sdApu = apu
            , sdBus = bus
            , sdCartRam = cartRam
            , sdCartMbc = cartMbc
            }

decodeCpu :: Snap.Cursor CpuState
decodeCpu = do
    a <- Snap.getU8
    f <- Snap.getU8
    b <- Snap.getU8
    c <- Snap.getU8
    d <- Snap.getU8
    e <- Snap.getU8
    h <- Snap.getU8
    l <- Snap.getU8
    sp <- Snap.getU16
    pc <- Snap.getU16
    ime <- Snap.getBool
    ei <- Snap.getBool
    halted <- Snap.getBool
    _pad <- Snap.getU8
    cycles <- Snap.getI64
    pure
        CpuState
            { cpuRegs =
                Registers
                    { regA = a
                    , regF = f
                    , regB = b
                    , regC = c
                    , regD = d
                    , regE = e
                    , regH = h
                    , regL = l
                    , regSP = sp
                    , regPC = pc
                    }
            , cpuIme = ime
            , cpuEiDelay = ei
            , cpuHalted = halted
            , cpuHaltBug = False -- transient one-instruction latch
            , cpuCycles = fromIntegral cycles
            }

decodeTimer :: Snap.Cursor TimerState
decodeTimer = do
    divider <- Snap.getU16
    tima <- Snap.getU8
    tma <- Snap.getU8
    tac <- Snap.getU8
    prevAnd <- Snap.getBool
    reload <- Snap.getU8
    reloaded <- Snap.getU8
    pure
        TimerState
            { timDivider = divider
            , timTima = tima
            , timTma = tma
            , timTac = tac
            , timPrevAnd = prevAnd
            , timReloadCounter = fromIntegral reload
            , timReloadedCounter = fromIntegral reloaded
            }

decodeJoypad :: Snap.Cursor (Word8, Word8, Bool)
decodeJoypad = (,,) <$> Snap.getU8 <*> Snap.getU8 <*> Snap.getBool

decodePpu :: Snap.Cursor PpuData
decodePpu = do
    lcdc <- Snap.getU8
    stat <- Snap.getU8
    ly <- Snap.getU8
    lyc <- Snap.getU8
    scy <- Snap.getU8
    scx <- Snap.getU8
    wy <- Snap.getU8
    wx <- Snap.getU8
    bgp <- Snap.getU8
    obp0 <- Snap.getU8
    obp1 <- Snap.getU8
    modeByte <- Snap.getU8
    dot <- Snap.getU32
    vram <- Snap.getBlob
    oam <- Snap.getBlob
    fb <- Snap.getBlob
    vbk <- Snap.getU8
    bcps <- Snap.getU8
    ocps <- Snap.getU8
    bgPal <- Snap.getBlob
    objPal <- Snap.getBlob
    wly <- Snap.getU32
    prevStat <- Snap.getBool
    pendStat <- Snap.getBool
    opri <- Snap.getU8
    lcdOnFirst <- Snap.getBool
    pure
        PpuData
            { pdLcdc = lcdc
            , pdStat = stat
            , pdLy = ly
            , pdLyc = lyc
            , pdScy = scy
            , pdScx = scx
            , pdWy = wy
            , pdWx = wx
            , pdBgp = bgp
            , pdObp0 = obp0
            , pdObp1 = obp1
            , -- The encoder only ever writes 0..3; anything else is a corrupt
              -- blob, so clamp instead of letting 'toEnum' throw.
              pdMode = decodePpuMode modeByte
            , pdDot = fromIntegral dot
            , pdVram = vram
            , pdOam = oam
            , pdFb = fb
            , pdVbk = vbk
            , pdBcps = bcps
            , pdOcps = ocps
            , pdBgPal = bgPal
            , pdObjPal = objPal
            , pdWindowLine = fromIntegral wly
            , pdPrevStat = prevStat
            , pdPendingStat = pendStat
            , pdOpri = opri .&. 0x01
            , pdLcdOnFirstLine = lcdOnFirst
            }

decodePpuMode :: Word8 -> Ppu.PpuMode
decodePpuMode 0 = Ppu.ModeHBlank
decodePpuMode 1 = Ppu.ModeVBlank
decodePpuMode 2 = Ppu.ModeOamScan
decodePpuMode _ = Ppu.ModeDrawing

decodeBus :: Snap.Cursor BusData
decodeBus = do
    wram <- Snap.getBlob
    hram <- Snap.getBlob
    io <- Snap.getBlob
    ie <- Snap.getU8
    wbk <- Snap.getU8
    key1 <- Snap.getU8
    hdmaSrc <- Snap.getU16
    hdmaDst <- Snap.getU16
    hdmaLen <- Snap.getU32
    hdmaActive <- Snap.getBool
    ds <- Snap.getBool
    dsAcc <- Snap.getU8
    oamActive <- Snap.getBool
    oamStarting <- Snap.getBool
    oamSrc <- Snap.getU16
    oamIndex <- Snap.getU8
    pure
        BusData
            { bdWram = wram
            , bdHram = hram
            , bdIo = io
            , bdIe = ie
            , bdWramBank = wbk
            , bdKey1 = key1
            , bdHdmaSrc = hdmaSrc
            , bdHdmaDst = hdmaDst
            , bdHdmaLen = fromIntegral hdmaLen
            , bdHdmaActive = hdmaActive
            , bdDoubleSpeed = ds
            , bdDoubleSpeedAcc = fromIntegral dsAcc
            , bdOamActive = oamActive
            , bdOamStarting = oamStarting
            , bdOamSrc = oamSrc
            , bdOamIndex = fromIntegral oamIndex
            }

applySnapshot :: SnapshotData -> Machine -> IO ()
applySnapshot sd m = do
    let bus = machineBus m
        ps = Bus.busPpu bus
        pd = sdPpu sd
        bd = sdBus sd
    writeIORef (machineCpu m) (sdCpu sd)
    writeIORef (Bus.busTimer bus) (sdTimer sd)
    Joypad.loadState (sdJoypad sd) (Bus.busJoypad bus)
    writeIORef (Ppu.ppuLcdc ps) (pdLcdc pd)
    writeIORef (Ppu.ppuStat ps) (pdStat pd)
    writeIORef (Ppu.ppuLy ps) (pdLy pd)
    writeIORef (Ppu.ppuLyc ps) (pdLyc pd)
    writeIORef (Ppu.ppuScy ps) (pdScy pd)
    writeIORef (Ppu.ppuScx ps) (pdScx pd)
    writeIORef (Ppu.ppuWy ps) (pdWy pd)
    writeIORef (Ppu.ppuWx ps) (pdWx pd)
    writeIORef (Ppu.ppuBgp ps) (pdBgp pd)
    writeIORef (Ppu.ppuObp0 ps) (pdObp0 pd)
    writeIORef (Ppu.ppuObp1 ps) (pdObp1 pd)
    writeIORef (Ppu.ppuMode ps) (pdMode pd)
    writeIORef (Ppu.ppuDot ps) (pdDot pd)
    writeBytesToVector (pdVram pd) (Ppu.ppuVram ps)
    writeBytesToVector (pdOam pd) (Ppu.ppuOam ps)
    writeBytesToVector (pdFb pd) (Ppu.ppuFb ps)
    writeIORef (Ppu.ppuVbk ps) (pdVbk pd)
    writeIORef (Ppu.ppuBcps ps) (pdBcps pd)
    writeIORef (Ppu.ppuOcps ps) (pdOcps pd)
    writeBytesToVector (pdBgPal pd) (Ppu.ppuBgPalRam ps)
    writeBytesToVector (pdObjPal pd) (Ppu.ppuObjPalRam ps)
    writeIORef (Ppu.ppuWindowLine ps) (pdWindowLine pd)
    writeIORef (Ppu.ppuPrevStatLine ps) (pdPrevStat pd)
    writeIORef (Ppu.ppuPendingStatIrq ps) (pdPendingStat pd)
    writeIORef (Ppu.ppuOpri ps) (pdOpri pd)
    -- Restore this before 'resyncMode3End': mode 3 starts at dot 76 rather than
    -- 80 on the short line, so the latch it rebuilds depends on this flag.
    writeIORef (Ppu.ppuLcdOnFirstLine ps) (pdLcdOnFirstLine pd)
    -- The mode 3 end latch is derived from the registers just restored, and is
    -- not part of the blob. Rebuild it so the line in progress does not finish
    -- on whatever the previous machine had latched.
    Ppu.resyncMode3End ps
    -- Any debt on the target machine belongs to a timeline we are discarding
    -- along with the rest of its state, so drop it rather than settling it
    -- into the freshly restored APU.
    Bus.discardApuDebt bus
    Apu.loadState (sdApu sd) (Bus.busApu bus)
    writeBytesToVector (bdWram bd) (Bus.busWram bus)
    writeBytesToVector (bdHram bd) (Bus.busHram bus)
    writeBytesToVector (bdIo bd) (Bus.busIo bus)
    writeIORef (Bus.busIe bus) (bdIe bd)
    writeIORef (Bus.busWramBank bus) (bdWramBank bd)
    writeIORef (Bus.busKey1 bus) (bdKey1 bd)
    writeIORef (Bus.busHdmaSrc bus) (bdHdmaSrc bd)
    writeIORef (Bus.busHdmaDst bus) (bdHdmaDst bd)
    writeIORef (Bus.busHdmaLen bus) (bdHdmaLen bd)
    writeIORef (Bus.busHdmaActive bus) (bdHdmaActive bd)
    writeIORef (Bus.busDoubleSpeed bus) (bdDoubleSpeed bd)
    writeIORef (Bus.busDoubleSpeedAcc bus) (bdDoubleSpeedAcc bd)
    writeIORef (Bus.busOamDmaActive bus) (bdOamActive bd)
    writeIORef (Bus.busOamDmaStarting bus) (bdOamStarting bd)
    writeIORef (Bus.busOamDmaSrc bus) (bdOamSrc bd)
    writeIORef (Bus.busOamDmaIndex bus) (bdOamIndex bd)
    Cart.loadSave (sdCartRam sd) (Bus.busCart bus)
    Cart.loadMbc (sdCartMbc sd) (Bus.busCart bus)

writeBytesToVector :: ByteString -> MV.IOVector Word8 -> IO ()
writeBytesToVector bs v = do
    let n = min (BS.length bs) (MV.length v)
    mapM_ (\i -> MV.write v i (BS.index bs i)) [0 .. n - 1]
