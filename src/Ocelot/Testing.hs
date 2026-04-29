{-# LANGUAGE BangPatterns #-}

{- | Library-side helpers for tests and ad-hoc exploration. The functions in
this module bypass the normal cartridge-loading flow to construct a
'Machine' from raw program bytes. They live in @IO@ now that the emulator
state is mutable.

These helpers must not be used in production paths: real ROM loading goes
through 'Ocelot.Cartridge.loadRom' and 'Ocelot.Machine.machineFromCartridge'.
-}
module Ocelot.Testing (
    machineWithProgram,
    synthNoMbcRom,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (newIORef)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (loadRom)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import Ocelot.Cpu.Registers (regPC)
import Ocelot.Cpu.State (CpuState (..), freshCpu)
import Ocelot.Machine (Machine (..))

{- | Build a 'Machine' seeded with the given program bytes loaded at
@0x0000@, with @PC=0x0000@ and @SP=0xFFFE@.
-}
machineWithProgram :: ByteString -> IO Machine
machineWithProgram bs = do
    let !rom = synthNoMbcRom bs
    result <- loadRom rom
    case result of
        Right cart -> do
            let cpu = freshCpu{cpuRegs = (cpuRegs freshCpu){regPC = 0x0000}}
            cpuRef <- newIORef cpu
            bus <- Bus.fromCartridge cart
            pure (Machine cpuRef bus)
        Left err ->
            error
                ( "Ocelot.Testing.machineWithProgram: synthetic ROM did not load: "
                    ++ show err
                )

{- | Build a 32 KiB NoMbc cartridge image with the given program bytes at
offset 0 and a valid header at @0x0134-0x014D@.
-}
synthNoMbcRom :: ByteString -> ByteString
synthNoMbcRom prog =
    let romSize = 32 * 1024
        progBytes = BS.unpack (BS.take 0x0100 prog)
        v0 = V.replicate romSize 0 :: V.Vector Word8
        progFields = zip [0 ..] progBytes
        headerFields =
            [ (0x0146, 0x00)
            , (0x0147, 0x00)
            , (0x0148, 0x00)
            , (0x0149, 0x00)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
        v1 = v0 V.// (progFields ++ headerFields)
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.pack (V.toList (v1 V.// [(0x014D, cs)]))
