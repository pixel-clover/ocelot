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
* Writes to the serial control register @0xFF02@ that start an internal-clock
  transfer capture the byte at @0xFF01@ into the serial output buffer and arm
  a 128 M-cycle countdown. @SC@ bit 7 stays set until that expires, at which
  point @SB@ reads @0xFF@ (the line idles high with no peer) and @IF@ bit 3 is
  raised. See 'stepSerial'.
* Writes to @0xFF46@ start an OAM DMA, which then copies one byte per CPU
  M-cycle for 160 M-cycles. While it runs the CPU is locked off OAM and off the
  one internal bus the transfer is using; see 'addrInDmaUse' and 'stepOamDma'.
* The unusable region @0xFEA0-0xFEFF@ ignores writes; reads return @0xFF@.
* @0xFF00@ (joypad) is routed to 'Ocelot.Joypad', which drives the active-low
  button matrix and latches the joypad interrupt edge.

The APU is the one peripheral 'advance' does not tick in lockstep; it is
deferred and settled on demand. See 'flushApu'.
-}
module Ocelot.Bus (
    Bus (..),
    fromCartridge,
    fromCartridgeOnHost,
    HostHardware (..),
    BootMode (..),
    cpuMCyclesPerLcdFrame,
    isCgb,
    isDoubleSpeed,
    takeFrameReady,
    read8,
    write8,
    advance,
    setButton,
    framebuffer,
    framebufferRgb,
    framebufferRgbBytes,
    framebufferRgbaBytes,
    framebufferRgbaPtr,
    copyFramebufferRgbWithPitch,
    copyFramebufferRgba,
    drainSerial,
    drainAudioSamples,
    drainAudioSamplesVector,
    drainAudioSamplesInto,
    triggerSpeedSwitch,
    resetTimerDiv,
    takeStallCycles,
    flushApu,
    triggerOamBug,
    discardApuDebt,
    installBootRom,
) where

import Control.Monad (when)
import Data.Bits (complement, setBit, shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
import Foreign.Ptr (Ptr)
import Ocelot.Apu (ApuState)
import qualified Ocelot.Apu as Apu
import Ocelot.Cartridge (Cartridge)
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Joypad (Button, JoypadState)
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
    , busSerialBitsLeft :: !(IORef Int)
    -- ^ Bits still to shift in an in-flight internal-clock serial transfer,
    -- or 0 when idle. Bits rather than a cycle countdown because the shift
    -- clock is a division of the DIV divider rather than something counted
    -- from the @SC@ write; 'stepSerial' has the reasoning.
    , busFrameReady :: !(IORef Bool)
    , busCgb :: !Bool
    -- ^ True when the host hardware is CGB. Gates CGB-only registers
    -- (VBK, BCPS, etc.). Note: this is currently host-driven, NOT
    -- cart-driven, so a DMG-only cart on CGB hardware sees the CGB
    -- I/O surface. See 'busCgbDmgCompat' for the DMG-compat carve-out.
    , busCgbDmgCompat :: !Bool
    -- ^ True when CGB hardware is running a DMG-only cart in
    -- "DMG-compat mode". Real CGB hardware disables certain CGB-only
    -- registers in this mode (KEY1 reads 0xFF, OPRI reads 0xFF, etc.),
    -- which mooneye's @misc/boot_hwio-C@ and @misc/bits/unused_hwio-C@
    -- verify. Computed as @busCgb && cart.cgbFlag == DmgOnly@.
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
    , busOamDmaRestarting :: !(IORef Bool)
    -- ^ Whether the in-flight transfer was triggered on top of one already
    -- running. SameBoy's @dma_restarting@: it keeps OAM closed through the new
    -- transfer's warm-up, where a fresh transfer leaves one readable M-cycle.
    -- Stays set for the rest of the transfer, which costs nothing because a
    -- non-zero index closes OAM anyway.
    , busOamDmaStarting :: !(IORef Bool)
    -- ^ True for one M-cycle between the FF46 write and the first byte
    -- copy. Models the documented "DMA starts after the cycle in which
    -- it was triggered" behavior.
    , busApuDebt :: !(IORef Int)
    -- ^ Peripheral M-cycles owed to the APU but not yet stepped. The APU is
    -- the one subsystem whose state nothing observes between accesses: it
    -- raises no interrupt and feeds nothing back into the bus, so its only
    -- observation channels are its own register window, the sample drain,
    -- and the snapshot dump. Settling the debt at those points (see
    -- 'flushApu') instead of stepping every M-cycle is bit-exact, because
    -- 'Ocelot.Apu.advance' chunks to the next event horizon and so never
    -- steps past an event. It avoids rebuilding the whole @ApuInternal@
    -- record once per M-cycle, which was the single largest source of
    -- allocation in the emulator.
    , busStallCycles :: !(IORef Int)
    -- ^ CPU M-cycles the bus consumed on the CPU's behalf during the
    -- current instruction (currently only general-mode HDMA, which stalls
    -- the CPU for the duration of the copy). 'Ocelot.Cpu.Execute' drains
    -- this after each instruction and folds it into @cpuCycles@; the
    -- peripherals have already been ticked, so it must not be advanced
    -- again.
    }

{- | Choice of host model for the emulated bus. Most CGB-only registers
(KEY1, VBK, BCPS\/BCPD, OCPS\/OCPD, HDMA1-5, SVBK) become inaccessible
under 'HostDmg' (reads return @0xFF@, writes are ignored), and the
PPU's render path swaps between greenish-DMG shades ('Ppu.RenderDmg')
and the full CGB color pipeline ('Ppu.RenderCgbFull').
-}
data HostHardware
    = -- | Original Game Boy / Game Boy Pocket. Greenish-DMG palette.
      HostDmg
    | -- | Game Boy Color. CGB-only registers fully accessible. DMG carts
      -- run in compatibility mode with the auto-palette.
      HostCgb
    deriving (Eq, Show)

{- | Whether to leave peripheral state at hardware power-on (LCD off,
APU off, palettes zero) or layer in the values a real boot ROM would
have written by the time it hands off at PC=0x100. The PowerOn variant
is required when running an actual boot ROM so the boot ROM observes
the same registers a real DMG/CGB does at reset; PostBoot is the
default shortcut used when running a cart directly.
-}
data BootMode
    = -- | LCDC=0, BGP/OBP=0, NR52 bit 7=0; the boot ROM (or test) sets
      -- everything itself.
      BootPowerOn
    | -- | LCDC=0x91, BGP=0xFC, OBP0/1=0xFF, APU on at NR50=0x77
      -- NR51=0xF3 (post-boot register handoff).
      BootPostBoot
    deriving (Eq, Show)

{- | Default constructor: pick the host hardware automatically based on
the cart's CGB flag (DMG-only carts get a DMG host, CGB-aware carts
get a CGB host). Uses 'BootPostBoot' (skip-the-boot-ROM shortcut). Use
'fromCartridgeOnHost' to override host or boot mode.
-}
fromCartridge :: Cartridge -> IO Bus
fromCartridge c =
    let host = case Header.hdrCgbFlag (Cartridge.cartridgeHeader c) of
            Header.DmgOnly -> HostDmg
            Header.DmgAndCgb -> HostCgb
            Header.CgbOnly -> HostCgb
     in fromCartridgeOnHost host BootPostBoot c

-- | CPU M-cycles needed to advance one LCD frame at the current speed.
cpuMCyclesPerLcdFrame :: Bus -> IO Int
cpuMCyclesPerLcdFrame b = do
    ds <- readIORef (busDoubleSpeed b)
    pure (if ds then 17556 * 2 else 17556)

isCgb :: Bus -> Bool
isCgb = busCgb

isDoubleSpeed :: Bus -> IO Bool
isDoubleSpeed = readIORef . busDoubleSpeed

takeFrameReady :: Bus -> IO Bool
takeFrameReady b =
    atomicModifyIORef' (busFrameReady b) clearFrameReady

clearFrameReady :: Bool -> (Bool, Bool)
clearFrameReady ready = (False, ready)

{- | Construct a bus with an explicit host-hardware choice. Lets you run
a DMG cart on a CGB host (matching real-hardware backwards
compatibility, with the auto-palette pre-loaded) or vice versa. The
'BootMode' parameter selects between hardware power-on state and the
post-boot-ROM register handoff.
-}
fromCartridgeOnHost :: HostHardware -> BootMode -> Cartridge -> IO Bus
fromCartridgeOnHost host bootMode c = do
    wram <- MV.replicate 0x8000 0
    hram <- MV.replicate 0x7F 0
    io <- MV.replicate 0x80 0
    ie <- newIORef 0
    timer <- newIORef Timer.initialTimer
    ppu <- Ppu.initialPpu
    apu <- Apu.initial
    joypad <- Joypad.initial
    serial <- newIORef []
    serialBitsLeft <- newIORef 0
    frameReady <- newIORef False
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
    oamDmaRestarting <- newIORef False
    apuDebt <- newIORef 0
    stallCycles <- newIORef 0
    let cgbCart = case Header.hdrCgbFlag (Cartridge.cartridgeHeader c) of
            Header.DmgOnly -> False
            Header.DmgAndCgb -> True
            Header.CgbOnly -> True
        hardwareCgb = host == HostCgb
        -- 'busCgb' gates CGB-only registers (KEY1, VBK, BCPS, ...). On a
        -- DMG host these always read 0xFF, so we tie this to the host
        -- rather than the cart. A DMG cart running on a CGB host still
        -- has the CGB I/O surface visible (just not used in compat mode).
        cgb = hardwareCgb
        renderMode
            | cgbCart && hardwareCgb = Ppu.RenderCgbFull
            | hardwareCgb = Ppu.RenderCgbCompat
            | otherwise = Ppu.RenderDmg
    Ppu.setCgbMode cgb ppu
    Ppu.setCgbRenderMode renderMode ppu
    -- APU power-off semantics follow the host. On a DMG host length
    -- counters are preserved across power-off; on a CGB host they are
    -- cleared, matching real hardware. Via 'fromCartridge' the host
    -- defaults to follow the cart's CGB flag, so the default flow ties
    -- APU mode to the cart and lets blargg dmg_sound (length-preserve)
    -- and cgb_sound (length-clear) both pass.
    Apu.setCgbMode cgb apu
    -- Post-boot register handoff: only applied when no boot ROM is going
    -- to run. With a boot ROM, the boot ROM is responsible for setting
    -- LCDC, BGP/OBP*, and the APU power/mixer registers itself. Pre-
    -- writing them here would, for example, leave LCDC=0x91 from
    -- 'initialPpu' so the PPU advances LY during the boot ROM's leading
    -- NOPs and any differential trace drifts in PPU state within
    -- ~1000 T-cycles of cart entry.
    case bootMode of
        BootPostBoot -> do
            -- APU: NR52 powered on, master volume 7/7, panning all-channels-both-sides.
            Apu.write8 0xFF26 0x80 apu
            Apu.write8 0xFF24 0x77 apu
            Apu.write8 0xFF25 0xF3 apu
            -- PPU: LCDC=0x91 (LCD on, BG on, tile data 0x8000, tile map 0x9800),
            -- BGP=0xFC, OBP0/1=0xFF. LCDC goes through 'seedLcdc' rather than
            -- 'write8' so the handoff is not mistaken for a fresh LCD enable,
            -- which would start the machine on the short first scanline.
            Ppu.seedLcdc 0x91 ppu
            Ppu.write8 0xFF47 0xFC ppu
            Ppu.write8 0xFF48 0xFF ppu
            Ppu.write8 0xFF49 0xFF ppu
        BootPowerOn -> pure ()
    -- DMG-on-CGB compat: pre-load CGB palette RAM with the auto palette
    -- so the DMG cart's BGP/OBP shades index into recognizable colors.
    -- Also seed OPRI=1 so the sprite-priority logic uses leftmost-X
    -- (DMG behavior); the real CGB boot ROM does this for unmodified
    -- DMG carts.
    when (renderMode == Ppu.RenderCgbCompat) $ do
        applyCompatPalette (Cartridge.cartridgeHeader c) ppu
        Ppu.write8 0xFF6C 0x01 ppu
    -- Post-boot CGB palette state: real hardware's boot ROM seeds all
    -- BG and OBJ palettes with the same grayscale ramp so CGB carts that
    -- take a moment to fill BCPS/BCPD don't show pure-white during early
    -- frames, and so carts that READ OBJ palette RAM during boot (e.g.
    -- Wario Land 3) see a deterministic non-0xFF value to back up. CGB
    -- carts that initialise their own palettes overwrite it.
    when (renderMode == Ppu.RenderCgbFull && bootMode == BootPostBoot) $ do
        mapM_
            (\palIdx -> writePalEntry (Ppu.ppuBgPalRam ppu) palIdx grayscaleAuto)
            [0 .. 7]
        mapM_
            (\palIdx -> writePalEntry (Ppu.ppuObjPalRam ppu) palIdx grayscaleAuto)
            [0 .. 7]
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
            , busSerialBitsLeft = serialBitsLeft
            , busFrameReady = frameReady
            , busCgb = cgb
            , busCgbDmgCompat = cgb && not cgbCart
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
            , busOamDmaRestarting = oamDmaRestarting
            , busApuDebt = apuDebt
            , busStallCycles = stallCycles
            }

{- | CPU-side bus read. An in-flight OAM DMA holds the CPU off OAM and off the one
internal bus it is using, per 'addrInDmaUse'; blocked reads return @0xFF@. The DMA
itself reads through 'readDmaSource' to bypass this gate.
-}
read8 :: Word16 -> Bus -> IO Word8
read8 addr b = do
    inDmaUse <- addrInDmaUse addr b
    if inDmaUse
        then pure 0xFF
        else read8Raw addr b

{- | Which internal bus an address sits on.

OAM DMA occupies exactly one of these at a time. That is why it does not lock the
CPU out of the whole address space: a DMA reading VRAM leaves the main bus usable,
and a DMA reading anywhere else leaves VRAM usable. Mirrors SameBoy's
@bus_for_addr@.

Note that CGB splitting WRAM onto 'BusRam' does not by itself make WRAM readable
during a main-bus DMA: 'conflictsWithDma' still blocks @0xC000@ and up unless the
DMA is sourcing VRAM.
-}
data MemBus
    = -- | ROM, cart RAM, and (on DMG) WRAM plus its echo.
      BusMain
    | -- | VRAM at @0x8000-0x9FFF@.
      BusVram
    | -- | WRAM and echo, on CGB only.
      BusRam
    deriving (Eq)

busForAddr :: Bool -> Word16 -> MemBus
{-# INLINE busForAddr #-}
busForAddr cgb addr
    | addr < 0x8000 = BusMain
    | addr < 0xA000 = BusVram
    | addr < 0xC000 = BusMain
    | otherwise = if cgb then BusRam else BusMain

{- | Whether an in-flight OAM DMA is occupying the bus that @addr@ sits on, given
the address the DMA is currently sourcing from.

Mirrors SameBoy's @is_addr_in_dma_use@ (@Core\/memory.c@). Two exemptions look
odd but are load-bearing: the DMA's own source address reads back normally, and
so does its echo alias, because a source at @0xE000@ and up is fetched through
@src .&. 0xDFFF@.

Getting this wrong in the permissive direction is invisible; getting it wrong in
the restrictive direction is not. A blanket lock on everything below @0xFF00@
made an instruction fetch from WRAM or echo RAM return @0xFF@, which the CPU
decoded as @RST 38h@, and all nine mooneye instruction-timing ROMs wedged at
@PC=0x38@ instead of reporting a verdict.
-}
conflictsWithDma :: Bool -> Word16 -> Word16 -> Bool
{-# INLINE conflictsWithDma #-}
conflictsWithDma cgb cur addr
    | cur == addr = False
    | cur >= 0xE000 && (cur .&. 0xDFFF) == addr = False
    | cgb && addr >= 0xC000 = busForAddr cgb cur /= BusVram
    | cgb && cur >= 0xE000 = busForAddr cgb addr /= BusVram
    | otherwise = busForAddr cgb addr == busForAddr cgb cur

{- | Whether an active OAM DMA makes @addr@ unreadable by the CPU.

Above @0xFE00@ this keeps the blanket lock: the I\/O page, HRAM, and IE
(@0xFF00@ and up) are on a separate bus and stay accessible, while OAM itself is
held off for the whole transfer. SameBoy gates OAM through a separate rule in its
read path rather than through @is_addr_in_dma_use@; the two agree except on the
M-cycle before the first byte lands, where SameBoy still allows the access.
Below @0xFE00@ the per-bus check in 'conflictsWithDma' decides.
-}
addrInDmaUse :: Word16 -> Bus -> IO Bool
{-# INLINE addrInDmaUse #-}
addrInDmaUse addr b
    | addr >= 0xFF00 = pure False
    | otherwise = do
        active <- readIORef (busOamDmaActive b)
        if not active
            then pure False
            else
                if addr >= 0xFE00
                    then do
                        -- OAM is not held for the whole transfer. SameBoy blocks it while
                        -- @dma_current_dest /= 0@, and that counter is the 0xFF sentinel on the
                        -- trigger cycle, wraps to 0 on the cycle before byte 0 lands, then counts up.
                        -- So there is exactly one readable cycle, just before the first write. Holding
                        -- OAM for the whole transfer made a ROM executing from OAM fetch 0xFF and
                        -- derail into RST 38h, which is what mooneye 'oam_dma_start' catches.
                        --
                        -- That readable cycle belongs to a *fresh* transfer only. Retriggering
                        -- 0xFF46 mid-transfer leaves the previous DMA running through the new
                        -- one's warm-up, so OAM never opens: mooneye 'oam_dma_start' documents
                        -- the restart case as "M = 0, 1: previous DMA is running (OAM *not*
                        -- accessible)" against the fresh case's readable M = 1, and it executes
                        -- from OAM to tell them apart. 'busOamDmaRestarting' is SameBoy's
                        -- @dma_restarting@.
                        starting <- readIORef (busOamDmaStarting b)
                        restarting <- readIORef (busOamDmaRestarting b)
                        idx <- readIORef (busOamDmaIndex b)
                        pure (starting || restarting || idx /= 0)
                    else do
                        -- 'busOamDmaStarting' is Ocelot's startup delay: the DMA has been requested
                        -- but has not taken the bus yet, which is SameBoy's warm-up.
                        --
                        -- SameBoy also exempts an in-progress HDMA, but that flag of its own
                        -- ('hdma_in_progress') is set and cleared inside a single 'GB_hdma_run', so it
                        -- covers one 16-byte chunk. Ocelot's 'busHdmaActive' is a latch held for the
                        -- whole transfer, so testing it here would switch the OAM DMA lockout off for
                        -- 128 HBlanks on a 0x800-byte HBlank transfer, and it would still never fire
                        -- for general-mode HDMA, which clears the latch before copying. There is no
                        -- equivalent flag to test, so this deliberately has no HDMA exemption.
                        starting <- readIORef (busOamDmaStarting b)
                        if starting
                            then pure False
                            else do
                                src <- readIORef (busOamDmaSrc b)
                                idx <- readIORef (busOamDmaIndex b)
                                -- 'stepOamDma' copies the byte at @src + idx@ and then bumps idx, so
                                -- this is the address the DMA is about to occupy the bus for. On the
                                -- deferred-clear M-cycle idx has already reached 160, an address the
                                -- DMA never reads, so hold the last real one instead.
                                let !cur = src + fromIntegral (min idx 159)
                                pure (conflictsWithDma (busCgb b) cur addr)

-- The blocking windows line up with neither mode boundary, and reads and writes do not
-- share edges, so the PPU owns all four: see 'Ppu.cpuCanReadOam' and its neighbours.
ppuCpuCanReadVram :: Bus -> IO Bool
ppuCpuCanReadVram b = Ppu.cpuCanReadVram (busPpu b)

ppuCpuCanWriteVram :: Bus -> IO Bool
ppuCpuCanWriteVram b = Ppu.cpuCanWriteVram (busPpu b)

ppuCpuCanReadOam :: Bus -> IO Bool
ppuCpuCanReadOam b = Ppu.cpuCanReadOam (busPpu b)

ppuCpuCanWriteOam :: Bus -> IO Bool
ppuCpuCanWriteOam b = Ppu.cpuCanWriteOam (busPpu b)

read8Raw :: Word16 -> Bus -> IO Word8
read8Raw addr b
    | addr <= 0x7FFF = bootRomOrCart addr b
    | addr <= 0x9FFF = do
        accessible <- ppuCpuCanReadVram b
        if accessible then Ppu.read8 addr (busPpu b) else pure 0xFF
    | addr <= 0xBFFF = Cartridge.read8 addr (busCart b)
    | addr <= 0xCFFF = MV.read (busWram b) (fromIntegral addr .&. 0x0FFF)
    | addr <= 0xDFFF = readUpperWram addr b
    | addr <= 0xFDFF = readEcho addr b
    | addr <= 0xFE9F = do
        Ppu.triggerOamBug addr (busPpu b)
        accessible <- ppuCpuCanReadOam b
        if accessible then Ppu.read8 addr (busPpu b) else pure 0xFF
    | addr <= 0xFEFF = Ppu.triggerOamBug addr (busPpu b) >> pure 0xFF
    | addr == 0xFF00 = Joypad.readP1 (busJoypad b)
    -- IF (0xFF0F): only the low 5 bits are real interrupt flags; the
    -- upper 3 bits always read as 1.
    | addr == 0xFF0F = (.|. 0xE0) <$> MV.read (busIo b) 0x0F
    | addr == 0xFF04 = Timer.readDiv <$> readIORef (busTimer b)
    | addr == 0xFF05 = Timer.readTima <$> readIORef (busTimer b)
    | addr == 0xFF06 = Timer.readTma <$> readIORef (busTimer b)
    | addr == 0xFF07 = Timer.readTac <$> readIORef (busTimer b)
    | addr >= 0xFF10 && addr <= 0xFF3F = flushApu b >> Apu.read8 addr (busApu b)
    -- DMA register (FF46): reads back the last-written source-high byte.
    | addr == 0xFF46 = MV.read (busIo b) 0x46
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.read8 addr (busPpu b)
    | addr == 0xFF4D = readKey1 b
    -- All CGB-only registers below: read 0xFF on a DMG host. KEY1 and
    -- the WRAM bank already gate themselves; the PPU-routed ones (VBK,
    -- BCPS, BCPD, OCPS, OCPD) and the HDMA control register need an
    -- explicit gate at the bus layer.
    | addr == 0xFF4F = if busCgb b then Ppu.read8 addr (busPpu b) else pure 0xFF
    -- HDMA1-4 are write-only on hardware; they always read 0xFF, on
    -- both DMG and CGB. HDMA5 reads transfer state (CGB) or 0xFF (DMG).
    | addr >= 0xFF51 && addr <= 0xFF54 = pure 0xFF
    | addr == 0xFF55 = readHdma5 b
    -- BCPS / OCPS: index registers, accessible in CGB-DMG-compat too
    -- (CGB boot ROM seeds them when running a DMG cart).
    | addr == 0xFF68 = if busCgb b then Ppu.read8 addr (busPpu b) else pure 0xFF
    | addr == 0xFF6A = if busCgb b then Ppu.read8 addr (busPpu b) else pure 0xFF
    -- BCPD / OCPD: palette-data ports. In DMG-compat mode they're
    -- locked off (mooneye @misc/bits/unused_hwio-C@ expects 0xFF reads
    -- via 'test_unmapped'); on full CGB they round-trip through
    -- palette RAM at the current BCPS/OCPS index.
    | addr == 0xFF69 =
        if busCgb b && not (busCgbDmgCompat b)
            then Ppu.read8 addr (busPpu b)
            else pure 0xFF
    | addr == 0xFF6B =
        if busCgb b && not (busCgbDmgCompat b)
            then Ppu.read8 addr (busPpu b)
            else pure 0xFF
    -- OPRI (sprite priority) on CGB-DMG-compat is unmapped (mooneye
    -- @misc/boot_hwio-C@ expects 0xFF). On full CGB it reads through
    -- the PPU (.|. 0xFE mask).
    | addr == 0xFF6C =
        if busCgb b && not (busCgbDmgCompat b)
            then Ppu.read8 addr (busPpu b)
            else pure 0xFF
    | addr == 0xFF70 = readWramBank b
    -- SB / SC (serial): SB stores its byte; SC's lower bits (transfer
    -- enable, internal clock) are stored, with bit 1 (clock speed) and
    -- bits 6..2 reading as 1 on real hardware.
    | addr == 0xFF01 = MV.read (busIo b) 0x01
    | addr == 0xFF02 = (.|. 0x7E) <$> MV.read (busIo b) 0x02
    -- CGB-only undocumented R/W registers. On DMG they read 0xFF; on
    -- CGB they round-trip the last byte written, with FF75 forcing
    -- bits 0-3 and 7 to 1 (only bits 4-6 are wired). Mooneye
    -- 'misc/bits/unused_hwio-C' verifies the round-trip.
    -- \$FF72 / $FF73: full-byte CGB R/W storage (mooneye
    -- @misc/bits/unused_hwio-C@ verifies the round-trip).
    | addr == 0xFF72 = if busCgb b then MV.read (busIo b) 0x72 else pure 0xFF
    | addr == 0xFF73 = if busCgb b then MV.read (busIo b) 0x73 else pure 0xFF
    -- \$FF74: documented as unused on CGB; reads always 0xFF regardless
    -- of writes (matches mooneye @misc/bits/unused_hwio-C@ which uses
    -- 'test_unmapped $FF74').
    | addr == 0xFF74 = pure 0xFF
    | addr == 0xFF75 =
        if busCgb b
            then (.|. 0x8F) <$> MV.read (busIo b) 0x75
            else pure 0xFF
    -- PCM12 / PCM34: read-only on CGB, exposing the current 4-bit DAC
    -- output of channels 1+2 / 3+4. We don't have per-T-cycle channel
    -- amplitude exposed, so we approximate by returning 0 (channels off
    -- or in their initial silent state). Mooneye @misc/bits/unused_hwio-C@
    -- and @misc/boot_hwio-C@ expect 0x00 reads here.
    | addr == 0xFF76 = if busCgb b then pure 0x00 else pure 0xFF
    | addr == 0xFF77 = if busCgb b then pure 0x00 else pure 0xFF
    -- Anything else in the I/O page is an unmapped / reserved register
    -- that reads back 0xFF on hardware (mooneye 'bits/unused_hwio').
    | addr <= 0xFF7F = pure 0xFF
    | addr <= 0xFFFE = MV.read (busHram b) (fromIntegral addr .&. 0x7F)
    | otherwise = readIORef (busIe b)

{- | CPU-side bus write. Like 'read8', this is gated by the OAM DMA
lockout: while DMA is in progress, writes from the CPU to non-HRAM
addresses are silently dropped (real hardware tristates the address
bus, so the write goes nowhere). Writes to the @0xFF46@ register
itself are still accepted via the I/O range, which lets a cart
restart an in-flight DMA per mooneye 'oam_dma_restart'.
-}
write8 :: Word16 -> Word8 -> Bus -> IO ()
write8 addr !v b = do
    inDmaUse <- addrInDmaUse addr b
    if inDmaUse
        then pure ()
        else do
            write8Raw addr v b
            -- PPU register writes (STAT, LYC, LCDC bit 7) can drive a
            -- low->high transition of the OR'd STAT line and must raise
            -- IF bit 1 right away. The PPU latches such edges into a
            -- pending flag; the bus consumes it here regardless of
            -- which addr was written.
            edge <- Ppu.takePendingStatIrq (busPpu b)
            when edge (setIfBit 1 b)

write8Raw :: Word16 -> Word8 -> Bus -> IO ()
write8Raw addr !v b
    | addr <= 0x7FFF = Cartridge.write8 addr v (busCart b)
    | addr <= 0x9FFF = do
        accessible <- ppuCpuCanWriteVram b
        when accessible (Ppu.write8 addr v (busPpu b))
    | addr <= 0xBFFF = Cartridge.write8 addr v (busCart b)
    | addr <= 0xCFFF = MV.write (busWram b) (fromIntegral addr .&. 0x0FFF) v
    | addr <= 0xDFFF = writeUpperWram addr v b
    | addr <= 0xFDFF = writeEcho addr v b
    | addr <= 0xFE9F = do
        Ppu.triggerOamBug addr (busPpu b)
        accessible <- ppuCpuCanWriteOam b
        when accessible (Ppu.write8 addr v (busPpu b))
    | addr <= 0xFEFF = Ppu.triggerOamBug addr (busPpu b)
    | addr == 0xFF00 = Joypad.writeP1 v (busJoypad b)
    | addr == 0xFF02 = handleSerialControl v b
    | addr == 0xFF04 = resetDivider b
    | addr == 0xFF05 = modifyIORef' (busTimer b) (Timer.writeTima v)
    | addr == 0xFF06 = modifyIORef' (busTimer b) (Timer.writeTma v)
    | addr == 0xFF07 = applyTimerWrite (Timer.writeTac v) b
    | addr == 0xFF26 = writeNr52 v b
    | addr >= 0xFF10 && addr <= 0xFF3F = flushApu b >> Apu.write8 addr v (busApu b)
    | addr == 0xFF46 = oamDma v b
    | addr >= 0xFF40 && addr <= 0xFF4B = Ppu.write8 addr v (busPpu b)
    | addr == 0xFF4D = writeKey1 v b
    | addr == 0xFF4F = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr >= 0xFF51 && addr <= 0xFF55 = writeHdmaReg addr v b
    | addr == 0xFF68 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF69 = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6A = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6B = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF6C = when (busCgb b) (Ppu.write8 addr v (busPpu b))
    | addr == 0xFF50 = writeBootRomLock v b
    | addr == 0xFF70 = writeWramBank v b
    | addr <= 0xFF7F = MV.write (busIo b) (fromIntegral addr .&. 0x7F) v
    | addr <= 0xFFFE = MV.write (busHram b) (fromIntegral addr .&. 0x7F) v
    | otherwise = writeIORef (busIe b) v

{- | Write to @SC@ (@0xFF02@). Setting bit 7 starts a transfer; bit 0
selects the internal clock.

The outgoing byte is captured into the serial output buffer straight away
(that buffer is the emulator's stand-in for a printer/link peer, and test
ROMs use it as their verdict channel), but the register-visible side of the
transfer is timed: @SC@ bit 7 stays set and no interrupt fires until all
eight bits have been shifted out. Arming only loads the bit counter; the
shifting itself is paced by 'stepSerial'. An external-clock transfer has no
peer to supply the clock, so it never completes.
-}
handleSerialControl :: Word8 -> Bus -> IO ()
handleSerialControl v b
    | testBit v 7 = do
        sb <- MV.read (busIo b) 0x01
        MV.write (busIo b) 0x02 v
        modifyIORef' (busSerialOut b) (sb :)
        writeIORef
            (busSerialBitsLeft b)
            (if testBit v 0 then serialTransferBits else 0)
    | otherwise = do
        MV.write (busIo b) 0x02 v
        writeIORef (busSerialBitsLeft b) 0

-- | Bits an internal-clock transfer shifts before it completes.
serialTransferBits :: Int
serialTransferBits = 8

{- | T-cycles between shift-clock edges on the internal clock.

The internal serial clock is 8192 Hz, and that is the *bit* rate, so a whole
byte takes @8 * 512 = 4096@ T-cycles rather than the 512 an earlier reading of
"8 bits at 8192 Hz" produced here. Getting this eight times too fast is what
made mooneye @acceptance\/serial\/boot_sclk_align-dmgABCmgb@ fire its interrupt
inside the first few loop iterations instead of the 145th.
-}
serialClockPeriod :: Int
serialClockPeriod = 512

{- | Tick an in-flight serial transfer. On completion the incoming byte
lands in @SB@ (@0xFF@ with no link peer, since the line idles high), @SC@
bit 7 clears, and @IF@ bit 3 is raised.

The shift clock is not counted from the @SC@ write. It is a division of the
same 16-bit divider that drives DIV, so its edges are fixed to the phase the
divider has held since reset and a transfer's first bit lands on the next
edge, however soon that is. That is what mooneye
@acceptance\/serial\/boot_sclk_align-dmgABCmgb@ checks, and its own comment
spells out: "clock edges align based on the *reset time*, not the time when SC
is written to".

An edge is a falling edge of divider bit 8, i.e. the divider crossing a
multiple of 'serialClockPeriod'. 'Bus.advance' has already stepped the timer by
the time this runs, so the window just covered is @(now - 4n, now]@. The
counter is 16 bits and 65536 is a whole number of periods, so a wrap adds no
spurious edge and the subtraction can run in 'Int' without special-casing it.
-}
stepSerial :: Int -> Bus -> IO ()
{-# INLINE stepSerial #-}
stepSerial n b = do
    bitsLeft <- readIORef (busSerialBitsLeft b)
    when (bitsLeft > 0) $ do
        ts <- readIORef (busTimer b)
        let !now = fromIntegral (Timer.timDivider ts) :: Int
            !before = now - 4 * n
            !edges =
                (now `div` serialClockPeriod) - (before `div` serialClockPeriod)
            !bitsLeft' = bitsLeft - edges
        if bitsLeft' > 0
            then writeIORef (busSerialBitsLeft b) bitsLeft'
            else do
                writeIORef (busSerialBitsLeft b) 0
                MV.write (busIo b) 0x01 0xFF
                sc <- MV.read (busIo b) 0x02
                MV.write (busIo b) 0x02 (sc .&. 0x7F)
                setIfBit 3 b

{- | Resolve the active upper-WRAM bank: bank 0 is treated as bank 1 on
real hardware, so the lower 4 KiB (always bank 0) is mirrored only when
the selector is 0. Returns the byte offset into 'busWram' for raw
@addr@ in the @0xD000-0xDFFF@ range.
-}
upperWramOffset :: Word16 -> Bus -> IO Int
upperWramOffset addr b = do
    sel <- readIORef (busWramBank b)
    let bank = let n = fromIntegral (sel .&. 0x07) in if n == 0 then 1 else n
    pure (bank * 0x1000 + (fromIntegral addr .&. 0x0FFF))

readUpperWram :: Word16 -> Bus -> IO Word8
readUpperWram addr b = do
    off <- upperWramOffset addr b
    MV.read (busWram b) off

writeUpperWram :: Word16 -> Word8 -> Bus -> IO ()
writeUpperWram addr v b = do
    off <- upperWramOffset addr b
    MV.write (busWram b) off v

{- | Echo region @0xE000-0xFDFF@ mirrors @0xC000-0xDDFF@ (the lower 8 KiB
of WRAM, with the upper half routed through the active CGB bank).
-}
readEcho :: Word16 -> Bus -> IO Word8
readEcho addr b
    | addr < 0xF000 = MV.read (busWram b) (fromIntegral addr .&. 0x0FFF)
    | otherwise = readUpperWram (addr - 0x2000) b

writeEcho :: Word16 -> Word8 -> Bus -> IO ()
writeEcho addr v b
    | addr < 0xF000 = MV.write (busWram b) (fromIntegral addr .&. 0x0FFF) v
    | otherwise = writeUpperWram (addr - 0x2000) v b

readWramBank :: Bus -> IO Word8
readWramBank b
    | not (busCgb b) = pure 0xFF
    -- DMG-compat: WRAM banking is locked to bank 1; the register reads
    -- as unmapped (0xFF) per mooneye @misc/boot_hwio-C@.
    | busCgbDmgCompat b = pure 0xFF
    | otherwise = (.|. 0xF8) <$> readIORef (busWramBank b)

writeWramBank :: Word8 -> Bus -> IO ()
writeWramBank v b =
    -- DMG-compat: WRAM banking is locked to bank 1 (matches SameBoy
    -- 'memory.c:680' and the symmetrical read-side gate). Without this,
    -- mooneye @misc/bits/unused_hwio-C@ writes 0xFF to FF70 as part
    -- of 'test_unmapped', which would set bank to 7 and the very next
    -- stack pop reads from bank-7's uninitialized area, sending PC to
    -- 0x0000 and into the RST 38 trap.
    when (busCgb b && not (busCgbDmgCompat b)) $
        writeIORef (busWramBank b) (v .&. 0x07)

{- | KEY1 read: bit 7 = current speed (1 = double-speed), bit 0 =
pending switch. Bits 1..6 read as 1.
-}
readKey1 :: Bus -> IO Word8
readKey1 b
    -- DMG hardware: KEY1 doesn't exist; reads 0xFF.
    | not (busCgb b) = pure 0xFF
    -- CGB-DMG-compat (CGB hardware running DMG-only cart): KEY1 is
    -- disabled and reads 0xFF (mooneye @misc/boot_hwio-C@ expects this).
    | busCgbDmgCompat b = pure 0xFF
    | otherwise = do
        prepare <- readIORef (busKey1 b)
        ds <- readIORef (busDoubleSpeed b)
        pure ((if ds then 0x80 else 0x00) .|. (prepare .&. 0x01) .|. 0x7E)

writeKey1 :: Word8 -> Bus -> IO ()
writeKey1 v b = when (busCgb b && not (busCgbDmgCompat b)) (writeIORef (busKey1 b) (v .&. 0x01))

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

----------------------------------------------------------------------
-- Deferred APU
----------------------------------------------------------------------

{- | Upper bound on outstanding APU debt, in peripheral M-cycles. A game
that never touches an APU register and never drains audio would otherwise
let the debt (and the queued samples it will produce) grow without limit.
Roughly 1 ms of emulated time: far below one frame, and far above the
batch size at which the per-call overhead stops mattering.
-}

{- | Forward a CPU address-bus touch of the OAM range to the PPU's DMG OAM-bug
model. The PPU decides whether it applies (DMG only, and only while it is scanning
OAM), so callers pass the address unconditionally.

This exists so 'Ocelot.Cpu.Execute' can report the address-bus instructions that
corrupt OAM without importing 'Ocelot.Ppu'.
-}
triggerOamBug :: Word16 -> Bus -> IO ()
triggerOamBug addr b = Ppu.triggerOamBug addr (busPpu b)

apuDebtHorizon :: Int
apuDebtHorizon = 1024

-- | Add to the outstanding APU debt, settling it if it hits the horizon.
accrueApuDebt :: Int -> Bus -> IO ()
{-# INLINE accrueApuDebt #-}
accrueApuDebt n b = do
    debt <- readIORef (busApuDebt b)
    let !debt' = debt + n
    if debt' >= apuDebtHorizon
        then do
            writeIORef (busApuDebt b) 0
            Apu.advance debt' (busApu b)
        else writeIORef (busApuDebt b) debt'

{- | Settle any outstanding APU debt so the APU is current as of now. Must
run before anything observes APU state: a register read or write, a sample
drain, or a snapshot dump. Cheap and idempotent when the debt is zero.
-}
flushApu :: Bus -> IO ()
{-# INLINE flushApu #-}
flushApu b = do
    debt <- readIORef (busApuDebt b)
    when (debt > 0) $ do
        writeIORef (busApuDebt b) 0
        Apu.advance debt (busApu b)

{- | Discard outstanding APU debt without settling it. Only for snapshot
load, where the APU state is being replaced wholesale and the debt belongs
to a timeline that no longer exists.
-}
discardApuDebt :: Bus -> IO ()
discardApuDebt b = writeIORef (busApuDebt b) 0

{- | Zero the timer's internal 16-bit divider. @STOP@ resets it on real
hardware, in both the plain-halt and the CGB speed-switch case. Routed
through 'Timer.writeDiv' so the falling-edge quirk (a high AND signal
dropping to 0 bumps TIMA once) still applies.
-}
resetTimerDiv :: Bus -> IO ()
resetTimerDiv = resetDivider

{- | T-cycles until the divider next clocks the APU frame sequencer.

The sequencer runs off a falling edge of DIV bit 4, so the edges land on multiples
of 8192 divider ticks. In double speed it uses bit 5 instead, doubling the divider
period, and the APU is handed the halved cycle count, so the two cancel and the
answer stays in the same range.
-}
untilNextFrameEdge :: Bool -> Word16 -> Int
untilNextFrameEdge double d =
    let !period = if double then 16384 else 8192
        !remaining = period - (fromIntegral d `mod` period)
     in if double then remaining `div` 2 else remaining

{- | Write NR52, realigning the frame sequencer when this powers the APU on.

Hardware resets the sequencer's step on power-on but keeps clocking it from the
divider, so the next step arrives at the next DIV edge rather than a full period
later. 'Apu.write8' cannot work that out on its own: the divider lives in the
timer, so the phase has to come from here.
-}
writeNr52 :: Word8 -> Bus -> IO ()
writeNr52 v b = do
    flushApu b
    before <- Apu.read8 0xFF26 (busApu b)
    Apu.write8 0xFF26 v (busApu b)
    when (not (testBit before 7) && testBit v 7) $ do
        ts <- readIORef (busTimer b)
        double <- readIORef (busDoubleSpeed b)
        Apu.alignFrameTimer (untilNextFrameEdge double (Timer.timDivider ts)) (busApu b)

{- | Zero the divider, realigning the APU frame sequencer to its new phase.

The sequencer is clocked by a falling edge of DIV bit 4 on hardware (internal
divider bit 12, or bit 13 in double speed so the wall-clock rate is unchanged), so
zeroing the divider drops that bit if it was set and clocks the sequencer once.
'Apu.divReset' also restarts the APU's period counter, since the next edge is a
full period after the reset either way.

The APU is settled first: its time is deferred in 'busApuDebt', and realigning the
sequencer before settling would apply the new phase at the wrong point in the APU's
timeline.
-}
resetDivider :: Bus -> IO ()
resetDivider b = do
    ts <- readIORef (busTimer b)
    double <- readIORef (busDoubleSpeed b)
    let !seqBit = if double then 13 else 12
        !falling = testBit (Timer.timDivider ts) seqBit
    flushApu b
    applyTimerWrite Timer.writeDiv b
    Apu.divReset falling (busApu b)

{- | Run a timer register write that can itself drive a TIMA overflow, latching
@IF@ bit 2 straight away when it does.

Not deferred like the divider-driven overflow in 'Timer.advance': a write lands
part-way through its M-cycle on hardware, so @IF@ is up by the instruction
boundary, and 'Ocelot.Machine.cycleWrite' leaves no cycle after the write to run
the reload state machine in. See 'Timer.writeFallingEdge'.
-}
applyTimerWrite :: (Timer.TimerState -> (Timer.TimerState, Bool)) -> Bus -> IO ()
applyTimerWrite f b = do
    ts <- readIORef (busTimer b)
    let (!ts', !fired) = f ts
    writeIORef (busTimer b) ts'
    when fired (setIfBit 2 b)

{- | Read and clear the CPU-stall debit the bus accrued during the current
instruction. See 'busStallCycles'.
-}
takeStallCycles :: Bus -> IO Int
takeStallCycles b = atomicModifyIORef' (busStallCycles b) clearStallCycles

clearStallCycles :: Int -> (Int, Int)
clearStallCycles n = (0, n)

{- | Called by the @STOP@ instruction. On a CGB cart with KEY1 bit 0
set, this toggles the double-speed bit and clears the prepare-switch
latch; otherwise it's a no-op (the caller still sets cpuHalted).
-}
triggerSpeedSwitch :: Bus -> IO Bool
triggerSpeedSwitch b
    | not (busCgb b) || busCgbDmgCompat b = pure False
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

{- | HDMA5 read:

* Active HBlank transfer: bit 7 = 0, low 7 bits = @(remaining \/ 16) - 1@.
* Cancelled HBlank transfer with bytes left: bit 7 = 1, low 7 bits =
  @(remaining \/ 16) - 1@. CGB games (and the CGB boot ROM) read this
  value to decide how to resume the transfer.
* Otherwise idle: @0xFF@.
-}
readHdma5 :: Bus -> IO Word8
readHdma5 b
    | not (busCgb b) = pure 0xFF
    -- DMG-compat: HDMA disabled (matches SameBoy 'memory.c:677' where
    -- HDMA5 returns 0xFF when not cgb_mode). mooneye
    -- @misc/bits/unused_hwio-C@ verifies via 'test_unmapped'.
    | busCgbDmgCompat b = pure 0xFF
    | otherwise = do
        active <- readIORef (busHdmaActive b)
        len <- readIORef (busHdmaLen b)
        let !count = fromIntegral ((len `div` 16) - 1) .&. 0x7F
        if active
            then pure count
            else
                if len > 0
                    then pure (0x80 .|. count)
                    else pure 0xFF

writeHdmaReg :: Word16 -> Word8 -> Bus -> IO ()
writeHdmaReg addr v b =
    -- DMG-compat: HDMA disabled (SameBoy 'memory.c:1721' returns from
    -- HDMA writes when not cgb_mode). Without this gate, mooneye
    -- @misc/bits/unused_hwio-C@ writes 0xFF to FF55 starting an HBlank
    -- DMA from src=0 to VRAM, which silently corrupts VRAM tile data
    -- and causes the test's failure-print path to hang.
    when (busCgb b && not (busCgbDmgCompat b)) $ case addr of
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
                then do
                    writeIORef (busHdmaActive b) True
                    -- Hardware does not wait for the next HBlank *entry* when the PPU is
                    -- already in mode 0: the first chunk goes immediately. SameBoy
                    -- @Core/memory.c:1729@ sets @hdma_on@ right here when
                    -- @(STAT & 3) == 0@. Waiting for the entry edge instead left every
                    -- transfer armed during an HBlank running one chunk behind, which
                    -- matters because CGB games drive HDMA once per scanline.
                    --
                    -- This reads the STAT *register* view, delayed mode bits included,
                    -- because that is the value SameBoy tests. With the LCD off the mode
                    -- bits read 0, so a transfer armed then also starts immediately, and
                    -- then stalls for want of further HBlanks exactly as hardware does.
                    --
                    -- SameBoy also excludes its @display_state == 7@, a sub-mode Ocelot's
                    -- line model has no equivalent for; that edge stays unmodelled.
                    stat <- Ppu.read8 0xFF41 (busPpu b)
                    when (stat .&. 0x03 == 0) (stepHdmaHBlank b)
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
    -- Record the stall so the CPU's cycle counter reflects the time the
    -- copy took. The peripherals were just advanced, so the CPU must not
    -- advance them again for these cycles.
    modifyIORef' (busStallCycles b) (+ blockCycles)

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

{- | Wrap an HDMA destination back into the 8 KiB VRAM window. Hardware
only wires the low 13 address bits of the destination pointer, so a
transfer that runs past @0x9FFF@ continues at @0x8000@ rather than
spilling into the cartridge RAM window (where 'Ppu.write8' would match
nothing and silently drop the byte).
-}
vramDest :: Word16 -> Word16
{-# INLINE vramDest #-}
vramDest addr = 0x8000 .|. (addr .&. 0x1FFF)

copyHdmaBytes :: Bus -> Word16 -> Word16 -> Int -> IO ()
copyHdmaBytes b src dst n =
    mapM_
        ( \i -> do
            -- HDMA is its own bus master and is not gated by the OAM-DMA
            -- CPU lockout: 'read8' honors 'busOamDmaActive' and would
            -- return 0xFF for non-HRAM sources whenever HDMA fires while
            -- OAM DMA is mid-copy. 'read8Raw' is the underlying memory
            -- read without that gate, matching SameBoy 'GB_hdma_run'
            -- which reads through 'GB_read_memory_internal' regardless
            -- of the current OAM-DMA state.
            byte <- read8Raw (src + fromIntegral i) b
            -- Direct VRAM write (respects current VBK) bypassing the
            -- bus dispatcher to avoid recursion.
            Ppu.write8 (vramDest (dst + fromIntegral i)) byte (busPpu b)
        )
        [0 .. n - 1]

advanceHdmaPointers :: Bus -> Int -> IO ()
advanceHdmaPointers b n = do
    modifyIORef' (busHdmaSrc b) (+ fromIntegral n)
    modifyIORef' (busHdmaDst b) (vramDest . (+ fromIntegral n))

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
    -- A write landing on an already-running transfer is a restart, and the transfer
    -- it interrupts keeps the OAM bus through the new one's warm-up. Latched here
    -- because resetting the index below erases the only other trace of it.
    wasActive <- readIORef (busOamDmaActive b)
    writeIORef (busOamDmaRestarting b) wasActive
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

{- | Step OAM DMA by @n@ M-cycles. Top-level rather than a @let@-bound loop
inside 'advance' so it cannot capture a closure on the (overwhelmingly
common) path where no transfer is in flight.
-}
stepOamDmaN :: Int -> Bus -> IO ()
stepOamDmaN n b = go n
  where
    go 0 = pure ()
    go !k = stepOamDma b >> go (k - 1)

stepOamDma :: Bus -> IO ()
stepOamDma b = do
    active <- readIORef (busOamDmaActive b)
    starting <- readIORef (busOamDmaStarting b)
    when (active && not starting) $ do
        idx <- readIORef (busOamDmaIndex b)
        if idx >= 160
            then -- All 160 bytes already copied. Defer clearing
            -- 'busOamDmaActive' to one cycle past the final byte
            -- copy so a CPU read scheduled at the same M-cycle as
            -- the last byte still sees the lockout (mooneye
            -- 'oam_dma_timing'). 'busOamDmaIndex >= 160' acts as
            -- the deferred-clear sentinel.
                writeIORef (busOamDmaActive b) False
            else do
                src <- readIORef (busOamDmaSrc b)
                -- Read directly from the underlying memory rather than
                -- through 'read8', so the DMA itself isn't subject to
                -- the lockout it imposes on the CPU.
                byte <- readDmaSource (src + fromIntegral idx) b
                MV.write (Ppu.ppuOam (busPpu b)) idx byte
                writeIORef (busOamDmaIndex b) (idx + 1)

{- | DMA-internal source read. Bypasses the 'busOamDmaActive' lockout that
'read8' applies to CPU accesses; otherwise routes the same way. (DMA is
the one bus master that can still see memory while it's running.)
-}
readDmaSource :: Word16 -> Bus -> IO Word8
readDmaSource addr b
    | addr <= 0x7FFF = bootRomOrCart addr b
    | addr <= 0x9FFF = do
        accessible <- ppuCpuCanReadVram b
        if accessible then Ppu.read8 addr (busPpu b) else pure 0xFF
    | addr <= 0xBFFF = Cartridge.read8 addr (busCart b)
    | addr <= 0xCFFF = MV.read (busWram b) (fromIntegral addr .&. 0x0FFF)
    | addr <= 0xDFFF = readUpperWram addr b
    | addr <= 0xFDFF = readEcho addr b
    -- 0xFE00-0xFFFF: out-of-range source addresses. Real hardware splits
    -- on host model (matches SameBoy 'GB_dma_run' lines 1890-1895):
    --   * CGB: every byte reads as 0xFF.
    --   * DMG: the source mirrors via 'src & ~0x2000' into the
    --     0xC000-0xDFFF WRAM window (so e.g. 0xFE00 -> 0xDE00, 0xFF00 ->
    --     0xDF00, 0xFFFF -> 0xDFFF). Includes the 0xFFxx tail, which
    --     used to return 0xFF in this emulator.
    | busHardwareCgb b = pure 0xFF
    | otherwise = readDmaSource (addr .&. complement 0x2000) b

{- | Advance time-driven subsystems by N M-cycles. Ticks Timer and PPU and
latches the Timer interrupt (bit 2) and VBlank (bit 0) into @IF@ at @0xFF0F@.

Double-speed mode splits the peripherals in two, per Pandocs KEY1. These run
twice as fast in wall-clock terms, i.e. keep their rate relative to CPU
M-cycles and see the unhalved count:

* the timer and divider,
* the serial port,
* OAM DMA.

These keep their usual wall-clock rate, i.e. see half as many M-cycles per
CPU instruction:

* the LCD controller,
* all sound timings and frequencies,
* HDMA (handled in 'runGeneralHdma', which doubles its CPU-cycle debit).
-}
advance :: Int -> Bus -> IO ()
advance mCycles b = do
    -- Halved count for the wall-clock-rate peripherals. Odd M-cycles carry
    -- over in a 0/1 accumulator so the halving does not lose time. Only a
    -- CGB host can ever be in double speed, and 'busCgb' is a pure field, so
    -- a DMG host skips the IORef read entirely.
    pCycles <-
        if not (busCgb b)
            then pure mCycles
            else do
                ds <- readIORef (busDoubleSpeed b)
                if not ds
                    then pure mCycles
                    else do
                        acc <- readIORef (busDoubleSpeedAcc b)
                        let total = acc + mCycles
                        writeIORef (busDoubleSpeedAcc b) (total `mod` 2)
                        pure (total `div` 2)
    -- The divider is clocked from the CPU clock, so DIV/TIMA keep their
    -- CPU-relative rate and take the unhalved count. Feeding the timer
    -- 'pCycles' ran every TAC rate at half speed for as long as a CGB game
    -- stayed in double-speed mode.
    ts <- readIORef (busTimer b)
    -- Scrutinise with 'case', not a lazy @let (ts', overflow) = ...@ pattern
    -- binding. 'overflow' is not demanded until the IF-latching block several
    -- statements below, which is far enough that the demand analyser did not
    -- fire: the binding allocated a pair thunk plus a selector thunk per
    -- component, and stored the new TimerState into the IORef as a thunk,
    -- every M-cycle. A strict case lets the worker/wrapper unbox the pair.
    (!ts', !overflow) <- pure (Timer.advance mCycles ts)
    writeIORef (busTimer b) ts'
    ppuIrqs <- Ppu.advance pCycles (busPpu b)
    -- The APU is deferred rather than stepped here; see 'flushApu'.
    accrueApuDebt pCycles b
    -- OAM DMA copies one byte per *CPU* M-cycle, NOT per peripheral
    -- M-cycle: the DMA controller is on the CPU side of the speed
    -- divider, so a 160 M-cycle CPU wait covers the whole transfer in
    -- both single-speed and double-speed mode. Running this at the
    -- halved 'pCycles' rate (as we used to) made OAM DMA take 320 CPU
    -- M-cycles in double-speed, so a CGB cart that wrote FF46 and busy-
    -- waited for ~160 cycles (Wario Land 3, Super Mario Bros. Deluxe,
    -- Zelda DX, etc.) returned from its DMA wait while DMA was still
    -- locked. RET then read 0xFF off the stack and crashed back to
    -- 0xFFFF, sending the cart through its watchdog reset and into a
    -- white-screen reboot loop. Matches SameBoy 'GB_advance_cycles'
    -- 'gb->dma_cycles = cycles' (line 455) which captures the count
    -- \*before* the single-speed 'cycles <<= 1' doubling.
    --
    -- The whole block is gated on the transfer being live at entry. When it
    -- is not, every 'stepOamDma' iteration would read two IORefs only to
    -- no-op, and the "starting" reset below would dirty a clean IORef once
    -- per M-cycle. 'busOamDmaStarting' is only ever set alongside
    -- 'busOamDmaActive', so an inactive DMA already has it clear.
    oamActive <- readIORef (busOamDmaActive b)
    when oamActive $ do
        stepOamDmaN mCycles b
        -- The "starting" flag holds the DMA off for the duration of the
        -- triggering instruction (we run advance after the instruction has
        -- already completed its register-store side-effect). Clearing it at
        -- the end of advance lets copying begin on the *next* instruction's
        -- first M-cycle, matching the documented 1-cycle startup delay.
        writeIORef (busOamDmaStarting b) False
    -- Serial, like OAM DMA, is clocked from the CPU side of the speed
    -- divider, so it also sees the unhalved count.
    stepSerial mCycles b
    when overflow (setIfBit 2 b) -- Timer
    when (testBit ppuIrqs 0) $ do
        writeIORef (busFrameReady b) True
        setIfBit 0 b -- VBlank
    when (testBit ppuIrqs 1) (setIfBit 1 b) -- LCD STAT
    -- HBlank-entered signal (bit 2): step one HDMA chunk, not an interrupt.
    when (testBit ppuIrqs 2) (stepHdmaHBlank b)
    -- Joypad IRQ pending edge (set by Joypad.setButton).
    jpEdge <- Joypad.takeIrqPending (busJoypad b)
    when jpEdge (setIfBit 4 b)

setButton :: Button -> Bool -> Bus -> IO ()
setButton button pressed b = Joypad.setButton button pressed (busJoypad b)

framebuffer :: Bus -> IO (Vector Word8)
framebuffer b = Ppu.framebuffer (busPpu b)

framebufferRgb :: Bus -> IO (Vector Word8)
framebufferRgb b = Ppu.framebufferRgb (busPpu b)

framebufferRgbBytes :: Bus -> IO ByteString
framebufferRgbBytes b = Ppu.framebufferRgbBytes (busPpu b)

framebufferRgbaBytes :: Bus -> IO ByteString
framebufferRgbaBytes b = Ppu.framebufferRgbaBytes (busPpu b)

copyFramebufferRgbWithPitch :: Ptr Word8 -> Int -> Bus -> IO ()
copyFramebufferRgbWithPitch ptr pitch b = Ppu.copyFramebufferRgbWithPitch ptr pitch (busPpu b)

copyFramebufferRgba :: Ptr Word8 -> Bus -> IO ()
copyFramebufferRgba ptr b = Ppu.copyFramebufferRgba ptr (busPpu b)

framebufferRgbaPtr :: Bus -> Ptr Word8
framebufferRgbaPtr b = Ppu.framebufferRgbaPtr (busPpu b)

-- | Drain the APU's pending stereo samples (interleaved L,R) for the frontend.
drainAudioSamples :: Bus -> IO [Int16]
drainAudioSamples b = flushApu b >> Apu.drainSamples (busApu b)

-- | Drain the APU's pending stereo samples into an immutable vector.
drainAudioSamplesVector :: Bus -> IO (Vector Int16)
drainAudioSamplesVector b = flushApu b >> Apu.drainSamplesVector (busApu b)

drainAudioSamplesInto :: Ptr Int16 -> Int -> Bus -> IO Int
drainAudioSamplesInto ptr capacity b =
    flushApu b >> Apu.drainSamplesInto ptr capacity (busApu b)

setIfBit :: Int -> Bus -> IO ()
{-# INLINE setIfBit #-}
setIfBit n b = do
    iflag <- MV.read (busIo b) 0x0F
    MV.write (busIo b) 0x0F (setBit iflag n)

drainSerial :: Bus -> IO [Word8]
drainSerial b = atomicModifyIORef' (busSerialOut b) (\bytes -> ([], reverse bytes))
