{- | The SM83 CPU's full per-instruction state: registers plus a few sticky
bits that live alongside (interrupt-enable master, halt, cycle counter).
-}
module Ocelot.Cpu.State (
    CpuState (..),
    dmgPostBootCpu,
    dmg0PostBootCpu,
    mgbPostBootCpu,
    sgbPostBootCpu,
    sgb2PostBootCpu,
    cgbPostBootCpu,
    cgbAPostBootCpu,
    cgbDmgCompatPostBootCpu,
    freshCpu,
) where

import Data.Word (Word64)
import Ocelot.Cpu.Registers (
    Registers,
    cgbAPostBoot,
    cgbDmgCompatPostBoot,
    cgbPostBoot,
    dmg0PostBoot,
    dmgPostBoot,
    mgbPostBoot,
    regSP,
    sgb2PostBoot,
    sgbPostBoot,
    zeroRegisters,
 )

data CpuState = CpuState
    { cpuRegs :: !Registers
    , cpuIme :: !Bool
    , cpuEiDelay :: !Bool
    -- ^ Set by @EI@ and consumed by 'Ocelot.Cpu.Execute.step': the master
    -- interrupt enable becomes effective only after the instruction
    -- following @EI@ completes.
    , cpuHalted :: !Bool
    , cpuHaltBug :: !Bool
    -- ^ Set when @HALT@ executes with @IME=0@ and a pending interrupt:
    -- the CPU does not actually halt and instead fails to advance @PC@
    -- on the very next instruction fetch (so the byte after @HALT@ is
    -- decoded twice). Cleared as soon as the next fetch consumes it.
    , cpuCycles :: !Word64
    }
    deriving (Eq, Show)

-- | CPU state immediately after the DMG boot ROM hands off to the cartridge.
dmgPostBootCpu :: CpuState
dmgPostBootCpu = CpuState dmgPostBoot False False False False 0

-- | CPU state immediately after the DMG0 boot ROM hands off.
dmg0PostBootCpu :: CpuState
dmg0PostBootCpu = CpuState dmg0PostBoot False False False False 0

-- | CPU state immediately after the MGB boot ROM hands off.
mgbPostBootCpu :: CpuState
mgbPostBootCpu = CpuState mgbPostBoot False False False False 0

-- | CPU state immediately after the SGB boot ROM hands off.
sgbPostBootCpu :: CpuState
sgbPostBootCpu = CpuState sgbPostBoot False False False False 0

-- | CPU state immediately after the SGB2 boot ROM hands off.
sgb2PostBootCpu :: CpuState
sgb2PostBootCpu = CpuState sgb2PostBoot False False False False 0

-- | CPU state immediately after the CGB boot ROM hands off to the cartridge.
cgbPostBootCpu :: CpuState
cgbPostBootCpu = CpuState cgbPostBoot False False False False 0

-- | CPU state on a CGB chip revision A booting a DMG-only cartridge.
cgbAPostBootCpu :: CpuState
cgbAPostBootCpu = CpuState cgbAPostBoot False False False False 0

-- | CPU state on a regular CGB booting a DMG-only cartridge.
cgbDmgCompatPostBootCpu :: CpuState
cgbDmgCompatPostBootCpu = CpuState cgbDmgCompatPostBoot False False False False 0

{- | All registers cleared, SP at @0xFFFE@, PC at @0x0000@. Convenient starting
point for unit tests that drop a hand-written program at offset 0.
-}
freshCpu :: CpuState
freshCpu =
    CpuState
        { cpuRegs = zeroRegisters{regSP = 0xFFFE}
        , cpuIme = False
        , cpuEiDelay = False
        , cpuHalted = False
        , cpuHaltBug = False
        , cpuCycles = 0
        }
