{-# LANGUAGE OverloadedStrings #-}

{- | Run a ROM and scan the framebuffer pixel-by-pixel, reporting how many pixels are non-white and
printing up to five sample non-white coordinates with their RGB.
-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)

defaultInstructions :: Int
defaultInstructions = 60_000_000

main :: IO ()
main = do
    args <- getArgs
    (path, instructions) <- case args of
        [p] -> pure (p, defaultInstructions)
        [p, n] -> pure (p, read n)
        _ -> putStrLn "usage: scan-fb <rom> [instructions]" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    m <- machineFromCartridge cart
    let bus = machineBus m

    _ <- runFor instructions m
    fb <- Ppu.framebufferRgb (Bus.busPpu bus)
    let total = V.length fb `div` 3
    let pixels =
            [ (x, y, r, g, b)
            | y <- [0 .. 143]
            , x <- [0 .. 159]
            , let i = y * 160 + x
                  r = fb V.! (i * 3)
                  g = fb V.! (i * 3 + 1)
                  b = fb V.! (i * 3 + 2)
            , (r, g, b) /= (255, 255, 255)
            ]
    putStrLn $ "non-white pixels: " <> show (length pixels) <> " of " <> show total
    mapM_ print (take 5 pixels)
