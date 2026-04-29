{-# LANGUAGE BangPatterns #-}

{- | The top-level 'Machine' record stitches the CPU together with the system
'Bus'. State is mutable: 'machineCpu' is an 'IORef' holding the pure
'CpuState' record, and 'machineBus' carries the bus's mutable buffers.
-}
module Ocelot.Machine (
    Machine (..),
    machineFromCartridge,
    readMem,
    writeMem,
    advanceBus,
    getCpuRegs,
    getCpu,
    putCpu,
    mapCpu,
    mapCpuRegs,
) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word16, Word8)
import Ocelot.Bus (Bus)
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge)
import Ocelot.Cpu.Registers (Registers, regPC)
import Ocelot.Cpu.State (CpuState (..), freshCpu)

data Machine = Machine
    { machineCpu :: !(IORef CpuState)
    , machineBus :: !Bus
    }

-- | Construct a Machine from a freshly loaded cartridge with PC=0x0100.
machineFromCartridge :: Cartridge -> IO Machine
machineFromCartridge c = do
    let cpu = freshCpu{cpuRegs = (cpuRegs freshCpu){regPC = 0x0100}}
    cpuRef <- newIORef cpu
    bus <- Bus.fromCartridge c
    pure (Machine cpuRef bus)

readMem :: Word16 -> Machine -> IO Word8
readMem addr m = Bus.read8 addr (machineBus m)

writeMem :: Word16 -> Word8 -> Machine -> IO ()
writeMem addr !v m = Bus.write8 addr v (machineBus m)

advanceBus :: Int -> Machine -> IO ()
advanceBus n m = Bus.advance n (machineBus m)

getCpu :: Machine -> IO CpuState
getCpu m = readIORef (machineCpu m)

putCpu :: CpuState -> Machine -> IO ()
putCpu c m = writeIORef (machineCpu m) c

getCpuRegs :: Machine -> IO Registers
getCpuRegs m = cpuRegs <$> readIORef (machineCpu m)

mapCpu :: (CpuState -> CpuState) -> Machine -> IO ()
mapCpu f m = modifyIORef' (machineCpu m) f

mapCpuRegs :: (Registers -> Registers) -> Machine -> IO ()
mapCpuRegs f m =
    modifyIORef' (machineCpu m) (\c -> c{cpuRegs = f (cpuRegs c)})
