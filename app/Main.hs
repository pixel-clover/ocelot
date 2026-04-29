{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket_, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Frontend.Sdl as Sdl
import Numeric (showHex)
import Ocelot (
    CartridgeError,
    Header (..),
    cartridgeHasBattery,
    extractSave,
    loadRom,
    loadSave,
    parseHeader,
    version,
 )
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (Cartridge)
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Cpu.Registers (regPC)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), getCpu, getCpuRegs, machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

stepCap :: Int
stepCap = 10000000

main :: IO ()
main = do
    TIO.putStrLn version
    args <- getArgs
    case args of
        [] -> printUsage
        ["--help"] -> printUsage
        ["-h"] -> printUsage
        ["--headless", path] -> describeRom path
        ["--audio-test"] -> Sdl.audioTest
        [path] -> playRom path
        _ -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
    putStrLn ""
    putStrLn "usage: ocelot [--headless | --audio-test] <rom-path>"
    putStrLn ""
    putStrLn "  Default: open an SDL window and run the ROM at 60 FPS."
    putStrLn "  --headless: run for a fixed number of instructions and dump the"
    putStrLn "              final framebuffer to the terminal."
    putStrLn "  --audio-test: play a 440 Hz sine tone for 2 seconds (no ROM"
    putStrLn "                needed; verifies the SDL audio path)."
    putStrLn ""
    putStrLn "  SDL key bindings:"
    putStrLn "    Z          A button"
    putStrLn "    X          B button"
    putStrLn "    Return     Start"
    putStrLn "    Right Shift Select"
    putStrLn "    Arrow keys D-pad"
    putStrLn "    Space      Pause toggle"
    putStrLn "    Tab (held) Fast-forward (4x)"
    putStrLn "    F5         Save state to <rom>.state"
    putStrLn "    F7         Load state from <rom>.state"
    putStrLn "    F12        Screenshot to <rom>-<timestamp>.ppm"
    putStrLn "    Escape     Quit"

playRom :: FilePath -> IO ()
playRom path = do
    bytes <- readRomBytes path
    case parseHeader bytes of
        Left err -> die ("invalid cartridge header: " <> show err)
        Right hdr -> do
            printHeader hdr
            result <- loadRom bytes
            case result of
                Right cart -> do
                    let savePath = path <> ".sav"
                        battery = cartridgeHasBattery cart
                    when battery (loadSaveIfExists savePath cart)
                    bracket_
                        (pure ())
                        (when battery (writeSave savePath cart))
                        (Sdl.play path cart (hdrTitle hdr))
                Left err -> printNotSupported err

loadSaveIfExists :: FilePath -> Cartridge.Cartridge -> IO ()
loadSaveIfExists path cart = do
    r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case r of
        Right bs -> do
            putStrLn ("save:     loading " <> path)
            loadSave bs cart
        Left _ -> putStrLn ("save:     no existing " <> path <> " (will create on exit)")

writeSave :: FilePath -> Cartridge.Cartridge -> IO ()
writeSave path cart = do
    bs <- extractSave cart
    BS.writeFile path bs
    putStrLn ("save:     wrote " <> path)

describeRom :: FilePath -> IO ()
describeRom path = do
    bytes <- readRomBytes path
    case parseHeader bytes of
        Left err -> die ("invalid cartridge header: " <> show err)
        Right hdr -> do
            printHeader hdr
            result <- loadRom bytes
            case result of
                Right cart -> runHeadless cart
                Left err -> printNotSupported err

readRomBytes :: FilePath -> IO BS.ByteString
readRomBytes path = do
    r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case r of
        Left e -> die ("could not read ROM file " <> path <> ": " <> show e)
        Right bytes -> pure bytes

printHeader :: Header -> IO ()
printHeader h = do
    TIO.putStrLn ("title:    " <> hdrTitle h)
    putStrLn ("mbc:      " <> show (hdrMbcKind h))
    putStrLn ("rom:      " <> show (hdrRomBytes h `div` 1024) <> " KiB")
    putStrLn ("ram:      " <> show (hdrRamBytes h `div` 1024) <> " KiB")
    putStrLn ("cgb flag: " <> show (hdrCgbFlag h))
    putStrLn ("sgb flag: " <> show (hdrSgbFlag h))

printNotSupported :: CartridgeError -> IO ()
printNotSupported err =
    putStrLn ("status:   not yet supported (" <> show err <> ")")

runHeadless :: Cartridge -> IO ()
runHeadless cart = do
    putStrLn ("status:   stepping for up to " <> show stepCap <> " instructions...")
    m0 <- machineFromCartridge cart
    n <- runFor stepCap m0
    cpu <- getCpu m0
    pc <- regPC <$> getCpuRegs m0
    serial <- Bus.drainSerial (machineBus m0)
    fb <- Ppu.framebuffer (Bus.busPpu (machineBus m0))
    putStrLn ("steps:    " <> show n)
    putStrLn ("halted:   " <> show (cpuHalted cpu))
    putStrLn ("PC:       0x" <> hex16 pc)
    case serial of
        [] -> putStrLn "serial:   (no output)"
        bs -> do
            putStr "serial:   "
            BS.putStr (BS.pack bs)
            putStrLn ""
    putStrLn ""
    putStrLn "framebuffer (160x144, sampled to 80x72):"
    renderFramebuffer fb

renderFramebuffer :: V.Vector Word8 -> IO ()
renderFramebuffer fb =
    mapM_
        (\y -> putStrLn [shadeChar (fb V.! (y * 160 + x)) | x <- [0, 2 .. 158]])
        [0, 2 .. 142]

shadeChar :: Word8 -> Char
shadeChar 0 = ' '
shadeChar 1 = '\x2591'
shadeChar 2 = '\x2592'
shadeChar 3 = '\x2588'
shadeChar _ = '?'

hex16 :: (Integral a) => a -> String
hex16 n =
    let s = showHex n ""
        pad = replicate (4 - length s) '0'
     in pad <> s

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
