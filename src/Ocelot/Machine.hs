{-# LANGUAGE BangPatterns #-}

{- | The top-level 'Machine' record stitches the CPU together with the system
'Bus'. State is mutable: 'machineCpu' is an 'IORef' holding the pure
'CpuState' record, and 'machineBus' carries the bus's mutable buffers.
-}
module Ocelot.Machine (
    Machine (..),
    machineFromCartridge,
    machineFromCartridgeWithBoot,
    readMem,
    writeMem,
    advanceBus,
    getCpuRegs,
    getCpu,
    putCpu,
    mapCpu,
    mapCpuRegs,
) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word16, Word8)
import Ocelot.Bus (Bus)
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge, cartridgeHeader)
import qualified Ocelot.Cartridge.Header as Header
import Ocelot.Cpu.Registers (Registers, regPC)
import Ocelot.Cpu.State (CpuState (..), cgbPostBootCpu, dmgPostBootCpu, freshCpu)

data Machine = Machine
    { machineCpu :: !(IORef CpuState)
    , machineBus :: !Bus
    }

{- | Construct a Machine from a freshly loaded cartridge with no boot
ROM. The CPU starts in the post-boot state for the cart's platform: a
DMG-only cart gets 'dmgPostBootCpu', and a CGB-aware cart gets
'cgbPostBootCpu' (notably @A = 0x11@, which is what CGB-aware ROMs
probe to decide whether to write the CGB palette pipeline).
-}
machineFromCartridge :: Cartridge -> IO Machine
machineFromCartridge = machineFromCartridgeWithBoot Nothing

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
    cpuRef <- newIORef cpu
    bus <- Bus.fromCartridge c
    case mBoot of
        Just rom -> Bus.installBootRom rom bus
        Nothing -> pure ()
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
