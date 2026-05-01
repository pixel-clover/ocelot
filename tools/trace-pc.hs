{-# LANGUAGE OverloadedStrings #-}

{- | Run a ROM, then sample the program counter (CPU instruction pointer) every 100 instructions
for 100,000 samples and print a histogram of the 30 hottest PCs. Useful for finding wait loops the
cart is stuck in. The "skip" parameter advances past initial setup before sampling.
-}
module Main (main) where

import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Numeric (showHex)
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Cpu.Registers (regPC)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), machineFromCartridge)
import System.Environment (getArgs)
import System.Exit (exitFailure)

defaultSkip :: Int
defaultSkip = 5_000_000

defaultSamples :: Int
defaultSamples = 100_000

defaultStride :: Int
defaultStride = 100

main :: IO ()
main = do
    args <- getArgs
    (path, skip, samples) <- case args of
        [p] -> pure (p, defaultSkip, defaultSamples)
        [p, s] -> pure (p, read s, defaultSamples)
        [p, s, n] -> pure (p, read s, read n)
        _ -> putStrLn "usage: trace-pc <rom> [skip-instructions] [samples]" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    m <- machineFromCartridge cart

    _ <- runFor skip m
    histRef <- newIORef Map.empty
    mapM_
        ( \_ -> do
            _ <- runFor defaultStride m
            cpu <- readIORef (machineCpu m)
            let pc = regPC (cpuRegs cpu)
            modifyIORef' histRef (Map.insertWith (+) pc 1)
        )
        [1 .. samples]

    hist <- readIORef histRef
    let entries = reverse . map (\(pc, n) -> (n, pc)) . Map.toList $ hist
    let topN = take 30 (reverse (rsort entries))
    putStrLn $ "Top 30 hot PCs over " <> show samples <> " samples:"
    mapM_ (\(n, pc) -> putStrLn $ "  PC=0x" <> showHex pc "" <> "  count=" <> show n) topN

rsort :: (Ord a) => [a] -> [a]
rsort = reverse . orderedAscending . reverse
  where
    orderedAscending = foldr insert []
    insert x [] = [x]
    insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys
