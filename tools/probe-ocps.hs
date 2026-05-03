{-# LANGUAGE OverloadedStrings #-}

-- One-off: load a ROM, run for N instructions, then write OCPS=k/read OCPD for k=0..7, and print results.
-- Used to verify CGB OBJ palette read path against the live Wario Land 3 boot state where the cart reads 0xFF
-- regardless of OCPS value.

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    (path, n) <- case args of
        [p] -> pure (p, 0 :: Int)
        [p, s] -> pure (p, read s)
        _ -> putStrLn "usage: probe-ocps <rom> [pre-instructions]" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    m <- machineFromCartridge cart
    let bus = machineBus m
        ppu = Bus.busPpu bus
    printf "busCgb=%s\n" (show (Bus.busCgb bus))
    _ <- runFor n m
    -- Read OCPS register state.
    ocps <- Bus.read8 0xFF6A bus
    printf "OCPS register reads: 0x%02X\n" ocps
    -- Direct vector inspection (bypasses OCPS/OCPD register path).
    printf "Direct ppuObjPalRam[0..7]: "
    mapM_ (\i -> MV.read (Ppu.ppuObjPalRam ppu) i >>= \b -> printf "%02X " b) [0 .. 7 :: Int]
    putStrLn ""
    -- Round-trip via OCPS+OCPD.
    printf "Via OCPS+OCPD     [0..7]: "
    mapM_
        ( \k -> do
            Bus.write8 0xFF6A k bus
            v <- Bus.read8 0xFF6B bus
            printf "%02X " v
        )
        [0 .. 7 :: Word8]
    putStrLn ""
