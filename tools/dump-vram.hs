{-# LANGUAGE OverloadedStrings #-}

{- | Run a ROM and dump VRAM tile data, BG tilemap, BG attribute map (CGB bank 1), and BG palette
RAM in hex. Useful for verifying the cart wrote data to the right place.
-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import Numeric (showHex)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)

defaultInstructions :: Int
defaultInstructions = 30_000_000

main :: IO ()
main = do
    args <- getArgs
    (path, instructions) <- case args of
        [p] -> pure (p, defaultInstructions)
        [p, n] -> pure (p, read n)
        _ -> putStrLn "usage: dump-vram <rom> [instructions]" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    m <- machineFromCartridge cart
    let bus = machineBus m
    let ppu = Bus.busPpu bus
    _ <- runFor instructions m

    let vram = Ppu.ppuVram ppu
    putStrLn "Tile data 0x8000-0x803F (first four tiles, bank 0):"
    bs1 <- mapM (\i -> MV.read vram i) [0 .. 0x3F]
    printChunked 16 0x8000 bs1

    putStrLn "BG tilemap 0x9800 (first 64 entries, bank 0):"
    bs2 <- mapM (\i -> MV.read vram (0x1800 + i)) [0 .. 63]
    printChunked 32 0x9800 bs2

    putStrLn "BG attribute map 0x9800 (CGB bank 1, first 64 entries):"
    bs3 <- mapM (\i -> MV.read vram (0x3800 + i)) [0 .. 63]
    printChunked 32 0x9800 bs3

    putStrLn "BG palette RAM (8 palettes x 4 colors x 2 bytes):"
    let bgPal = Ppu.ppuBgPalRam ppu
    bs4 <- mapM (\i -> MV.read bgPal i) [0 .. 63]
    printChunked 8 0 bs4

    putStrLn "OBJ palette RAM (8 palettes x 4 colors x 2 bytes):"
    let objPal = Ppu.ppuObjPalRam ppu
    bs5 <- mapM (\i -> MV.read objPal i) [0 .. 63]
    printChunked 8 0 bs5

printChunked :: Int -> Int -> [Word8] -> IO ()
printChunked w base bs =
    mapM_
        ( \(off, line) ->
            putStrLn $
                "  0x" <> showHex (base + off * w) "" <> ": "
                    <> unwords [showHex b "" | b <- line]
        )
        (zip [0 ..] (chunks w bs))
  where
    chunks _ [] = []
    chunks n xs = take n xs : chunks n (drop n xs)
