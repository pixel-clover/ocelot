{-# LANGUAGE BangPatterns #-}

{- | The top-level 'Machine' record stitches the CPU together with the
placeholder flat memory. Once subsystem modules (PPU, APU, Timer, Cartridge
routing) land, those become additional fields on this record and 'readMem' /
'writeMem' route through 'Ocelot.Bus' instead of going straight to
'Ocelot.Memory'.
-}
module Ocelot.Machine (
    Machine (..),
    initialMachine,
    machineWithProgram,

    -- * Memory accessors used by the CPU
    readMem,
    writeMem,

    -- * Register accessors used by the executor
    mapCpuRegs,
    mapCpu,
    getCpuRegs,
) where

import Data.ByteString (ByteString)
import Data.Word (Word16, Word8)
import Ocelot.Cpu.Registers (Registers, regPC)
import Ocelot.Cpu.State (CpuState (..), freshCpu)
import Ocelot.Memory (Memory)
import qualified Ocelot.Memory as Memory

data Machine = Machine
    { machineCpu :: !CpuState
    , machineMem :: !Memory
    }
    deriving (Eq, Show)

initialMachine :: Machine
initialMachine = Machine freshCpu Memory.initialMemory

{- | Construct a Machine with the given program bytes loaded at @0x0000@ and
@PC@ pointing at @0x0000@. SP starts at @0xFFFE@ as in 'freshCpu'.
-}
machineWithProgram :: ByteString -> Machine
machineWithProgram bs =
    let cpu = freshCpu{cpuRegs = (cpuRegs freshCpu){regPC = 0x0000}}
     in Machine cpu (Memory.fromBytes bs)

readMem :: Word16 -> Machine -> Word8
readMem addr m = Memory.read8 addr (machineMem m)

writeMem :: Word16 -> Word8 -> Machine -> Machine
writeMem addr !v m = m{machineMem = Memory.write8 addr v (machineMem m)}

getCpuRegs :: Machine -> Registers
getCpuRegs = cpuRegs . machineCpu

mapCpuRegs :: (Registers -> Registers) -> Machine -> Machine
mapCpuRegs f m =
    m{machineCpu = (machineCpu m){cpuRegs = f (cpuRegs (machineCpu m))}}

mapCpu :: (CpuState -> CpuState) -> Machine -> Machine
mapCpu f m = m{machineCpu = f (machineCpu m)}
