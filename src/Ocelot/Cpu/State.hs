{- | The SM83 CPU's full per-instruction state: registers plus a few sticky
bits that live alongside (interrupt-enable master, halt, cycle counter).
-}
module Ocelot.Cpu.State (
    CpuState (..),
    dmgPostBootCpu,
    freshCpu,
) where

import Data.Word (Word64)
import Ocelot.Cpu.Registers (Registers, dmgPostBoot, regSP, zeroRegisters)

data CpuState = CpuState
    { cpuRegs :: !Registers
    , cpuIme :: !Bool
    , cpuHalted :: !Bool
    , cpuCycles :: !Word64
    }
    deriving (Eq, Show)

-- | CPU state immediately after the DMG boot ROM hands off to the cartridge.
dmgPostBootCpu :: CpuState
dmgPostBootCpu = CpuState dmgPostBoot False False 0

{- | All registers cleared, SP at @0xFFFE@, PC at @0x0000@. Convenient starting
point for unit tests that drop a hand-written program at offset 0.
-}
freshCpu :: CpuState
freshCpu =
    CpuState
        { cpuRegs = zeroRegisters{regSP = 0xFFFE}
        , cpuIme = False
        , cpuHalted = False
        , cpuCycles = 0
        }
