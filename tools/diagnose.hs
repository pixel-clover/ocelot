{-# LANGUAGE OverloadedStrings #-}

{- | Run a ROM headlessly for a configurable number of CPU instructions and print a synopsis of CPU,
PPU, APU, and timer state, plus a small slice of the framebuffer. First probe when a cart wedges
with a white screen or goes silent.
-}
module Main (main) where

import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Numeric (showHex)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Cpu.Registers (regPC, regSP)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)

defaultInstructions :: Int
defaultInstructions = 5_000_000

main :: IO ()
main = do
    args <- getArgs
    (path, instructions) <- case args of
        [p] -> pure (p, defaultInstructions)
        [p, n] -> pure (p, read n)
        _ -> putStrLn "usage: diagnose <rom> [instructions]" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    m <- machineFromCartridge cart

    _ <- runFor instructions m
    let bus = machineBus m

    cpu <- readIORef (machineCpu m)
    let regs = cpuRegs cpu
    putStrLn $ "rom:    " <> path
    putStrLn $ "ran:    " <> show instructions <> " instructions"
    putStrLn $ "PC=0x" <> showHex (regPC regs) "" <> " SP=0x" <> showHex (regSP regs) ""
    putStrLn $ "halted=" <> show (cpuHalted cpu) <> " ime=" <> show (cpuIme cpu)

    iflag <- Bus.read8 0xFF0F bus
    ie <- Bus.read8 0xFFFF bus
    putStrLn $ "IF=0x" <> showHex iflag "" <> " IE=0x" <> showHex ie ""

    lcdc <- Bus.read8 0xFF40 bus
    stat <- Bus.read8 0xFF41 bus
    ly <- Bus.read8 0xFF44 bus
    lyc <- Bus.read8 0xFF45 bus
    putStrLn $ "LCDC=0x" <> showHex lcdc "" <> " STAT=0x" <> showHex stat ""
    putStrLn $ "LY=" <> show ly <> " LYC=" <> show lyc

    nr52 <- Bus.read8 0xFF26 bus
    nr50 <- Bus.read8 0xFF24 bus
    nr51 <- Bus.read8 0xFF25 bus
    putStrLn $ "NR50=0x" <> showHex nr50 "" <> " NR51=0x" <> showHex nr51 "" <> " NR52=0x" <> showHex nr52 ""

    fbRgb <- Ppu.framebufferRgb (Bus.busPpu bus)
    let total = V.length fbRgb `div` 3
        nonWhite =
            length
                [ ()
                | i <- [0 .. total - 1]
                , let r = fbRgb V.! (i * 3)
                      g = fbRgb V.! (i * 3 + 1)
                      b = fbRgb V.! (i * 3 + 2)
                , (r, g, b) /= (255, 255, 255)
                ]
    putStrLn $ "framebuffer: " <> show nonWhite <> " non-white pixels of " <> show total
    let pixel i =
            ( fbRgb V.! (i * 3)
            , fbRgb V.! (i * 3 + 1)
            , fbRgb V.! (i * 3 + 2)
            )
    putStrLn $ "  top-left:    " <> show (pixel 0)
    putStrLn $ "  mid-screen:  " <> show (pixel (80 + 72 * 160))
    putStrLn $ "  bottom-right:" <> show (pixel (159 + 143 * 160))
