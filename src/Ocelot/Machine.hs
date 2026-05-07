{-# LANGUAGE BangPatterns #-}

{- | The top-level 'Machine' record stitches the CPU together with the system
'Bus'. State is mutable: 'machineCpu' is an 'IORef' holding the pure
'CpuState' record, and 'machineBus' carries the bus's mutable buffers.
-}
module Ocelot.Machine (
    Machine (..),
    Variant (..),
    machineFromCartridge,
    machineFromCartridgeForcedCgb,
    machineFromCartridgeForcedDmg,
    machineFromCartridgeAsVariant,
    machineFromCartridgeAsVariantWithDiv,
    machineFromCartridgeForBootHwio,
    machineFromCartridgeWithBoot,
    readMem,
    writeMem,
    advanceBus,
    advanceBusInline,
    cycleRead,
    cycleWrite,
    cycleNoAccess,
    getCpuRegs,
    getCpu,
    putCpu,
    mapCpu,
    mapCpuRegs,
) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
import Ocelot.Bus (Bus)
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge, cartridgeHeader)
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Cpu.Registers (Registers)
import Ocelot.Cpu.State (
    CpuState (..),
    cgbAPostBootCpu,
    cgbDmgCompatPostBootCpu,
    cgbPostBootCpu,
    dmg0PostBootCpu,
    dmgPostBootCpu,
    freshCpu,
    mgbPostBootCpu,
    sgb2PostBootCpu,
    sgbPostBootCpu,
 )
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Timer as Timer

data Machine = Machine
    { machineCpu :: !(IORef CpuState)
    , machineBus :: !Bus
    , machineInternalAdvance :: !(IORef Int)
    -- ^ M-cycles already advanced from inside the current instruction's
    -- execution. Reset to 0 by 'doInstruction' before dispatch and
    -- subtracted from the instruction's reported cycle count when
    -- finishing the outer 'advanceBus'. Lets timed instructions
    -- (CALL, PUSH, RST, etc.) interleave bus ticks with their memory
    -- writes without double-counting.
    }

{- | Construct a Machine from a freshly loaded cartridge with no boot
ROM. The CPU starts in the post-boot state for the cart's platform: a
DMG-only cart gets 'dmgPostBootCpu', and a CGB-aware cart gets
'cgbPostBootCpu' (notably @A = 0x11@, which is what CGB-aware ROMs
probe to decide whether to write the CGB palette pipeline).
-}
machineFromCartridge :: Cartridge -> IO Machine
machineFromCartridge = machineFromCartridgeWithBoot Nothing

{- | Construct a Machine that runs the cart on CGB hardware regardless
of the cart's CGB flag. Used by mooneye-misc tests that target CGB
hardware behavior (e.g. @misc/bits/unused_hwio-C@) but ship as
DMG-flagged ROMs because the test code itself doesn't need CGB-only
opcodes — it just checks CGB-only register bits. The CPU starts in
@cgbPostBootCpu@ state and the bus uses @HostCgb@.
-}
machineFromCartridgeForcedCgb :: Cartridge -> IO Machine
machineFromCartridgeForcedCgb c = do
    cpuRef <- newIORef cgbPostBootCpu
    bus <- Bus.fromCartridgeOnHost Bus.HostCgb Bus.BootPostBoot c
    internalAdvance <- newIORef 0
    pure (Machine cpuRef bus internalAdvance)

{- | Construct a Machine that runs the cart on DMG hardware regardless
of the cart's CGB flag. Used by blargg test ROMs that ship with
@CGB flag = 0x80@ ("DMG/CGB compatible") but whose test logic is
written for DMG behavior — most blargg ROMs detect CGB at runtime
and either skip the test or use a different output path that we
don't render. Forcing DMG keeps the serial-based verdict path live.
-}
machineFromCartridgeForcedDmg :: Cartridge -> IO Machine
machineFromCartridgeForcedDmg c = do
    cpuRef <- newIORef dmgPostBootCpu
    bus <- Bus.fromCartridgeOnHost Bus.HostDmg Bus.BootPostBoot c
    internalAdvance <- newIORef 0
    pure (Machine cpuRef bus internalAdvance)

{- | Hardware variant for the per-variant @boot_regs-*@ tests.
mooneye ships these tests pre-built with @CGB flag = 0x00@ regardless
of the variant the test is meant to validate, so the cart header
alone can't tell us whether to seed @DMG0@ vs @MGB@ vs @SGB@ vs
@CGB-A@ post-boot register state. Tests that target a specific
hardware revision route through 'machineFromCartridgeAsVariant'.
-}
data Variant
    = VarDmgABC
    | VarDmg0
    | VarMgb
    | VarSgb
    | VarSgb2
    | -- | Regular CGB (B-E revision) booting a DMG-only cart in DMG-compat mode.
      VarCgbDmg
    | -- | CGB chip revision A booting a DMG-only cart.
      VarCgbA
    deriving (Eq, Show)

{- | Construct a Machine seeded with a specific hardware variant's
post-boot CPU state. The bus host follows the variant family (DMG,
MGB, SGB → 'HostDmg'; CGB → 'HostCgb'). Drives mooneye
@acceptance/boot_regs-{dmg0,mgb,sgb,sgb2}@ and
@misc/boot_regs-{A,cgb}@.
-}
machineFromCartridgeAsVariant :: Variant -> Cartridge -> IO Machine
machineFromCartridgeAsVariant v c = do
    let (cpu, host) = case v of
            VarDmgABC -> (dmgPostBootCpu, Bus.HostDmg)
            VarDmg0 -> (dmg0PostBootCpu, Bus.HostDmg)
            VarMgb -> (mgbPostBootCpu, Bus.HostDmg)
            VarSgb -> (sgbPostBootCpu, Bus.HostDmg)
            VarSgb2 -> (sgb2PostBootCpu, Bus.HostDmg)
            VarCgbDmg -> (cgbDmgCompatPostBootCpu, Bus.HostCgb)
            VarCgbA -> (cgbAPostBootCpu, Bus.HostCgb)
    cpuRef <- newIORef cpu
    bus <- Bus.fromCartridgeOnHost host Bus.BootPostBoot c
    internalAdvance <- newIORef 0
    pure (Machine cpuRef bus internalAdvance)

{- | Like 'machineFromCartridgeAsVariant' but also seeds the timer's
internal 16-bit divider counter to a specific value. mooneye's
@boot_div-*@ tests check the post-boot @DIV@ value AND its sub-byte
phase relative to subsequent CPU instructions, so they need both the
right register seed (variant) and the right counter handoff state
that the boot ROM would have left behind. Each per-variant counter
is derived from working backwards from the test's first @assert_b@
(see comments in 'Test.Ocelot.GoldenSpec.bootDivHandoff').
-}
machineFromCartridgeAsVariantWithDiv :: Variant -> Word16 -> Cartridge -> IO Machine
machineFromCartridgeAsVariantWithDiv v counter c = do
    m <- machineFromCartridgeAsVariant v c
    modifyIORef' (Bus.busTimer (machineBus m)) (\t -> t{Timer.timDivider = counter})
    pure m

{- | Boot-handoff seed for mooneye's @boot_hwio-*@ tests: variant +
divider counter + the I/O register state the real boot ROM leaves
behind (the "bing" sound's APU writes, IF bit 0, DMA reads-as-0xFF,
etc.). Test verifies a sweep of @0xFF00..0xFF7F@ + @0xFFFF@ against
hand-written expected tables; covering each register involves
seeding what the boot ROM wrote, not running the boot ROM.

The @ly0@ argument seeds the PPU's @LY@ at handoff. mooneye's tests
assert specific @LY@ values at the moment @0xFF44@ is read mid-sweep
(@0x0A@ on DMG-ABC, @0x01@ on DMG0). The sweep takes ~10 lines, so
the handoff @LY@ for each variant differs accordingly. We set the
PPU into @ModeVBlank@ at @ly0@ when @ly0 >= 144@ so the rendering
pipeline doesn't fire frame events during the test.
-}
machineFromCartridgeForBootHwio :: Variant -> Word16 -> Word8 -> Int -> Cartridge -> IO Machine
machineFromCartridgeForBootHwio v counter ly0 dot0 c = do
    m <- machineFromCartridgeAsVariantWithDiv v counter c
    let bus = machineBus m
    -- IF: real boot ROM leaves bit 0 (VBlank) latched after the
    -- final HALT-and-wait that ends the boot sequence (mooneye expects
    -- IF read as 0xE1 = 0xE0 unused mask | 0x01).
    Bus.write8 0xFF0F 0x01 bus
    -- DMA register: post-boot reads back 0xFF on real hardware (write
    -- side-effect aside). Seed the I/O byte directly so we don't
    -- trigger an actual DMA via the FF46 write path.
    MV.write (Bus.busIo bus) 0x46 0xFF
    -- NR32: bits 5-6 default to 00 (mute) at power-on. mooneye expects
    -- 0x9F read; our default ('initialWave' wvVolumeShift = 0) reads
    -- 0xBF (= 100%). Override here without touching initialWave so we
    -- don't perturb other APU tests that rely on the existing default.
    Bus.write8 0xFF1C 0x00 bus
    case v of
        VarSgb -> do
            -- SGB boot ROM leaves P1 with row-select bits high (read 0xFF)
            -- and writes NR11/NR12 to set duty/envelope but does NOT
            -- trigger CH1 via NR14, so NR52 reads 0xF0 not 0xF1.
            Bus.write8 0xFF00 0x30 bus
            Bus.write8 0xFF11 0x80 bus
            Bus.write8 0xFF12 0xF3 bus
        VarSgb2 -> do
            Bus.write8 0xFF00 0x30 bus
            Bus.write8 0xFF11 0x80 bus
            Bus.write8 0xFF12 0xF3 bus
        VarCgbDmg -> do
            -- CGB DMG-compat boot leaves P1 with row-select bits high
            -- (read 0xFF). It also writes BCPS=0x88 (auto-increment +
            -- palette index 8) and OCPS=0x90 (auto-increment + index
            -- 0x10), and triggers CH1 like DMG. mooneye expects FF68
            -- read 0xC8, FF6A 0xD0, FF76/FF77 0x00.
            Bus.write8 0xFF00 0x30 bus
            -- APU "bing" trigger
            Bus.write8 0xFF11 0x80 bus
            Bus.write8 0xFF12 0xF3 bus
            Bus.write8 0xFF13 0xC1 bus
            Bus.write8 0xFF14 0x87 bus
            -- CGB palette index registers
            Bus.write8 0xFF68 0x88 bus
            Bus.write8 0xFF6A 0x90 bus
        _ -> do
            -- DMG family: APU "bing" sound writes that the boot ROM
            -- leaves visible in the APU registers (NR11=0xBF, NR12=0xF3,
            -- NR14=0xBF post-trigger). The CH1 trigger via NR14=0x87
            -- also sets NR52 bit 0 (CH1 active), giving NR52=0xF1.
            Bus.write8 0xFF11 0x80 bus
            Bus.write8 0xFF12 0xF3 bus
            Bus.write8 0xFF13 0xC1 bus
            Bus.write8 0xFF14 0x87 bus
    -- PPU LY/mode handoff: each variant's boot ROM finishes at a
    -- different LY because it ran for a different number of cycles
    -- with the LCD on. Seed accordingly so mid-sweep $FF44 reads
    -- match the test's expected LY.
    let ppu = Bus.busPpu bus
    writeIORef (Ppu.ppuLy ppu) ly0
    writeIORef (Ppu.ppuDot ppu) dot0
    writeIORef (Ppu.ppuMode ppu) $
        if ly0 >= 144
            then Ppu.ModeVBlank
            else case dot0 of
                d | d < 80 -> Ppu.ModeOamScan
                d | d < 252 -> Ppu.ModeDrawing
                _ -> Ppu.ModeHBlank
    pure m

{- | Like 'machineFromCartridge' but optionally installs a boot ROM. If
a boot ROM is supplied, the CPU starts at PC=0 with cleared registers
('freshCpu' with SP=0xFFFE), and the bus serves the boot-ROM-mapped
ranges from the supplied bytes until the ROM writes a non-zero value
to @0xFF50@ to hand off to the cartridge.
-}
machineFromCartridgeWithBoot :: Maybe ByteString -> Cartridge -> IO Machine
machineFromCartridgeWithBoot mBoot c = do
    let cpu = case mBoot of
            Just _ -> freshCpu -- boot ROM will set its own initial state
            Nothing -> case Header.hdrCgbFlag (cartridgeHeader c) of
                Header.DmgOnly -> dmgPostBootCpu
                Header.DmgAndCgb -> cgbPostBootCpu
                Header.CgbOnly -> cgbPostBootCpu
        bootMode = case mBoot of
            Just _ -> Bus.BootPowerOn
            Nothing -> Bus.BootPostBoot
        host = case Header.hdrCgbFlag (cartridgeHeader c) of
            Header.DmgOnly -> Bus.HostDmg
            Header.DmgAndCgb -> Bus.HostCgb
            Header.CgbOnly -> Bus.HostCgb
    cpuRef <- newIORef cpu
    bus <- Bus.fromCartridgeOnHost host bootMode c
    case mBoot of
        Just rom -> Bus.installBootRom rom bus
        Nothing -> pure ()
    internalAdvance <- newIORef 0
    pure (Machine cpuRef bus internalAdvance)

readMem :: Word16 -> Machine -> IO Word8
{-# INLINE readMem #-}
readMem addr m = Bus.read8 addr (machineBus m)

writeMem :: Word16 -> Word8 -> Machine -> IO ()
{-# INLINE writeMem #-}
writeMem addr !v m = Bus.write8 addr v (machineBus m)

advanceBus :: Int -> Machine -> IO ()
{-# INLINE advanceBus #-}
advanceBus n m = Bus.advance n (machineBus m)

{- | Variant of 'advanceBus' for use inside instruction-level handlers
that need to interleave bus ticks with their memory accesses (CALL,
PUSH, RST, ...). Ticks the bus by @n@ M-cycles AND records the
advance so that the dispatcher in 'Ocelot.Cpu.Execute.doInstruction'
subtracts it from the instruction's cycle count, avoiding a
double-advance.
-}
advanceBusInline :: Int -> Machine -> IO ()
{-# INLINE advanceBusInline #-}
advanceBusInline n m = do
    Bus.advance n (machineBus m)
    modifyIORef' (machineInternalAdvance m) (+ n)

{- | Cycle-accurate memory read: tick the bus by 1 M-cycle, then read
the address. The read latches the bus state from the END of the
ticked cycle, matching SameBoy's @cycle_read@ semantics. The internal
advance counter is bumped so the dispatcher does not double-tick.
-}
cycleRead :: Word16 -> Machine -> IO Word8
{-# INLINE cycleRead #-}
cycleRead addr m = do
    advanceBusInline 1 m
    Bus.read8 addr (machineBus m)

{- | Cycle-accurate memory write: tick the bus by 1 M-cycle, then write
the byte. Mirrors 'cycleRead' for the write path.
-}
cycleWrite :: Word16 -> Word8 -> Machine -> IO ()
{-# INLINE cycleWrite #-}
cycleWrite addr !v m = do
    advanceBusInline 1 m
    Bus.write8 addr v (machineBus m)

-- | Tick the bus by 1 M-cycle without an access (internal cycles).
cycleNoAccess :: Machine -> IO ()
{-# INLINE cycleNoAccess #-}
cycleNoAccess = advanceBusInline 1

getCpu :: Machine -> IO CpuState
{-# INLINE getCpu #-}
getCpu m = readIORef (machineCpu m)

putCpu :: CpuState -> Machine -> IO ()
{-# INLINE putCpu #-}
putCpu c m = writeIORef (machineCpu m) c

getCpuRegs :: Machine -> IO Registers
{-# INLINE getCpuRegs #-}
getCpuRegs m = cpuRegs <$> readIORef (machineCpu m)

mapCpu :: (CpuState -> CpuState) -> Machine -> IO ()
{-# INLINE mapCpu #-}
mapCpu f m = modifyIORef' (machineCpu m) f

mapCpuRegs :: (Registers -> Registers) -> Machine -> IO ()
{-# INLINE mapCpuRegs #-}
mapCpuRegs f m =
    modifyIORef' (machineCpu m) (\c -> c{cpuRegs = f (cpuRegs c)})
