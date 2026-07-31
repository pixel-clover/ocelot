{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Run a blargg test ROM and print its verdict text verbatim.

The golden suite reports a blargg ROM as a bare pass/fail, which is enough for a
regression gate but throws away the most useful diagnostic the ROM emits: blargg
ROMs name the individual subtest that failed, either on the serial port or in the
result area at @0xA000@. Recovering that name is much cheaper than reconstructing
the same information from a differential trace.

> make tools
> bin/tools/blargg-run external/gb-test-roms/dmg_sound/rom_singles/07-len\ sweep\ period\ sync.gb

Prints every byte the ROM sent to the serial port, then the @0xA000@ result code.
@--dmg@ / @--cgb@ force the host model the way @GoldenSpec.mooneyeHost@ does for
the ROMs whose verdict path only works on one of the two; without a flag the
cart header decides, matching 'Machine.machineFromCartridge'.

Note the serial text is printed even on a timeout, because a hung ROM has usually
already named the subtest it hung inside.
-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Numeric (showHex)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Machine (
    Machine (..),
    machineFromCartridge,
    machineFromCartridgeForcedCgb,
    machineFromCartridgeForcedDmg,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)

{- | Matches 'GoldenSpec.blarggCap' and 'GoldenSpec.pollChunk' so a verdict here
means the same thing as a verdict in the test suite.
-}
defaultCap :: Int
defaultCap = 80_000_000

pollChunk :: Int
pollChunk = 1_000_000

main :: IO ()
main = do
    args <- getArgs
    (host, path, cap) <- case args of
        ["--dmg", p] -> pure (Just False, p, defaultCap)
        ["--cgb", p] -> pure (Just True, p, defaultCap)
        ["--dmg", p, c] -> pure (Just False, p, read c)
        ["--cgb", p, c] -> pure (Just True, p, read c)
        [p] -> pure (Nothing, p, defaultCap)
        [p, c] -> pure (Nothing, p, read c)
        _ -> putStrLn "usage: blargg-run [--dmg|--cgb] <rom> [m-cycle-cap]" >> exitFailure
    hSetBuffering stdout LineBuffering
    bytes <- BS.readFile path
    r <- Cartridge.loadRom bytes
    case r of
        Left e -> putStrLn ("loadRom: " <> show e) >> exitFailure
        Right cart -> do
            m <- case host of
                Just True -> machineFromCartridgeForcedCgb cart
                Just False -> machineFromCartridgeForcedDmg cart
                Nothing -> machineFromCartridge cart
            (serial, code, ran) <- run cap m
            putStrLn "--- serial ---"
            putStrLn (BSC.unpack serial)
            putStrLn "--- result ---"
            putStrLn ("0xA000 = 0x" <> showHex code "")
            putStrLn (if ran >= cap then "verdict: TIMEOUT" else "verdict: settled")

{- | Poll the serial port and @0xA000@ in chunks, stopping as soon as either
reports a verdict. Mirrors 'GoldenSpec.runUntilMemOrSerialVerdict', except that
it accumulates and returns the serial text rather than reducing it to a Bool.
-}
run :: Int -> Machine -> IO (BS.ByteString, Word8, Int)
run cap m = go 0 BS.empty
  where
    go !n !serial
        | n >= cap = do
            code <- Bus.read8 0xA000 (machineBus m)
            pure (serial, code, n)
        | otherwise = do
            _ <- runFor pollChunk m
            chunk <- Bus.drainSerial (machineBus m)
            code <- Bus.read8 0xA000 (machineBus m)
            let serial' = serial <> BS.pack chunk
                settled =
                    "Passed" `BS.isInfixOf` serial'
                        || "Failed" `BS.isInfixOf` serial'
                        -- 0xFF is what a cart-RAM-less ROM returns forever, so it is not a verdict.
                        || (code /= 0x80 && code /= 0xFF)
            if settled then pure (serial', code, n) else go (n + pollChunk) serial'
