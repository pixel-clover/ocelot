{- | The SM83 CPU's full per-instruction state: registers plus a few sticky
bits that live alongside (interrupt-enable master, halt, cycle counter).
-}
module Ocelot.Cpu.State (
    CpuState (..),
    dmgPostBootCpu,
    cgbPostBootCpu,
    freshCpu,
) where

import Data.Word (Word64)
import Ocelot.Cpu.Registers (Registers, cgbPostBoot, dmgPostBoot, regSP, zeroRegisters)

data CpuState = CpuState
    { cpuRegs :: !Registers
    , cpuIme :: !Bool
    , cpuEiDelay :: !Bool
    -- ^ Set by @EI@ and consumed by 'Ocelot.Cpu.Execute.step': the master
    -- interrupt enable becomes effective only after the instruction
    -- following @EI@ completes.
    , cpuHalted :: !Bool
    , cpuCycles :: !Word64
    }
    deriving (Eq, Show)

-- | CPU state immediately after the DMG boot ROM hands off to the cartridge.
dmgPostBootCpu :: CpuState
dmgPostBootCpu = CpuState dmgPostBoot False False False 0

-- | CPU state immediately after the CGB boot ROM hands off to the cartridge.
cgbPostBootCpu :: CpuState
cgbPostBootCpu = CpuState cgbPostBoot False False False 0

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
        , cpuCycles = 0
        }
