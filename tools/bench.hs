{- | Throughput benchmark for the emulation core.

Mirrors the SDL frontend's per-frame path: 'runUntilFrame', then the RGB
framebuffer copy and the audio drain. Reports frames per second and the
multiple of real hardware speed (the Game Boy LCD runs at ~59.7 Hz), so a
result below @1.00x@ means the emulator cannot keep up with the console.

Build with @make tools@, then:

> bin/tools/bench test/testroms/cgb-acid2.gbc
> bin/tools/bench --frames 1200 --runs 5 test/testroms/dmg-acid2.gb
> bin/tools/bench --mode noblit test/testroms/cgb-acid2.gbc   # emulation only

Timings vary by a few percent run to run, so the median of several runs is
reported rather than a single sample. For allocation and GC behaviour, add
@+RTS -s@ (the tools rule builds with @-rtsopts@).
-}
module Main (main) where

import Control.Monad (forM_, replicateM)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.List (sort)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runUntilFrame)
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Ppu as Ppu
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Game Boy LCD refresh rate. The divisor for the "x realtime" figure.
lcdRefreshHz :: Double
lcdRefreshHz = 59.7275

-- | What to include in the measured window.
data Mode
    = -- | Emulation plus the frontend blit and audio drain.
      ModeFull
    | -- | Emulation only; isolates the core from frontend costs.
      ModeNoBlit
    deriving (Eq, Show)

data Options = Options
    { optFrames :: !Int
    , optRuns :: !Int
    , optMode :: !Mode
    , optRoms :: ![FilePath]
    }

defaultOptions :: Options
defaultOptions =
    Options{optFrames = 600, optRuns = 3, optMode = ModeFull, optRoms = []}

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args defaultOptions of
        Left err -> do
            hPutStrLn stderr ("bench: " <> err)
            hPutStrLn stderr usage
            exitFailure
        Right opts
            | null (optRoms opts) -> do
                hPutStrLn stderr "bench: no ROM given"
                hPutStrLn stderr usage
                exitFailure
            | otherwise -> forM_ (optRoms opts) (benchRom opts)

usage :: String
usage =
    unlines
        [ "usage: bench [--frames N] [--runs N] [--mode full|noblit] <rom>..."
        , "  --frames N        frames per run (default 600)"
        , "  --runs N          runs to take the median of (default 3)"
        , "  --mode full       emulation + framebuffer blit + audio drain (default)"
        , "  --mode noblit     emulation only"
        ]

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] opts = Right opts{optRoms = reverse (optRoms opts)}
parseArgs ("--frames" : n : rest) opts =
    readPositive "--frames" n >>= \v -> parseArgs rest opts{optFrames = v}
parseArgs ("--runs" : n : rest) opts =
    readPositive "--runs" n >>= \v -> parseArgs rest opts{optRuns = v}
parseArgs ("--mode" : "full" : rest) opts = parseArgs rest opts{optMode = ModeFull}
parseArgs ("--mode" : "noblit" : rest) opts = parseArgs rest opts{optMode = ModeNoBlit}
parseArgs ("--mode" : m : _) _ = Left ("unknown mode: " <> m)
parseArgs (flag : _) _ | take 2 flag == "--" = Left ("unknown flag: " <> flag)
parseArgs (rom : rest) opts = parseArgs rest opts{optRoms = rom : optRoms opts}

readPositive :: String -> String -> Either String Int
readPositive flag s = case reads s of
    [(v, "")] | v > 0 -> Right v
    _ -> Left (flag <> " expects a positive integer, got: " <> s)

benchRom :: Options -> FilePath -> IO ()
benchRom opts path = do
    bytes <- BS.readFile path
    loaded <- Cartridge.loadRom bytes
    case loaded of
        Left err -> hPutStrLn stderr (path <> ": " <> show err)
        Right cart -> do
            fbBuf <- mallocBytes (Ppu.framebufferWidth * Ppu.framebufferHeight * 3)
            audioBuf <- mallocBytes (audioCapacity * 2 * 2)
            samples <- replicateM (optRuns opts) (oneRun opts cart fbBuf audioBuf)
            let fps = median samples
            putStrLn
                ( path
                    <> "  "
                    <> show (optFrames opts)
                    <> " frames x"
                    <> show (optRuns opts)
                    <> "  mode="
                    <> (if optMode opts == ModeFull then "full" else "noblit")
                    <> "  median "
                    <> showFFloat (Just 0) fps ""
                    <> " fps ("
                    <> showFFloat (Just 2) (fps / lcdRefreshHz) ""
                    <> "x realtime)"
                )

-- | Interleaved stereo samples the drain buffer can hold; ~0.5 s at 48 kHz.
audioCapacity :: Int
audioCapacity = 65536

{- | One timed run on a fresh machine. A short warm-up runs first so one-time
lazy setup and the ROM's boot sequence stay outside the timed window.
-}
oneRun :: Options -> Cartridge.Cartridge -> Ptr a -> Ptr b -> IO Double
oneRun opts cart fbBuf audioBuf = do
    m <- machineFromCartridge cart
    let bus = machineBus m
    -- Match the SDL frontend, which only reads the RGB framebuffer.
    Ppu.setFbTarget Ppu.FbRgb (Bus.busPpu bus)
    forM_ [1 .. warmupFrames] (\_ -> oneFrame opts bus m fbBuf audioBuf)
    t0 <- getMonotonicTime
    forM_ [1 .. optFrames opts] (\_ -> oneFrame opts bus m fbBuf audioBuf)
    t1 <- getMonotonicTime
    pure (fromIntegral (optFrames opts) / (t1 - t0))
  where
    warmupFrames = 30 :: Int

oneFrame :: Options -> Bus.Bus -> Machine -> Ptr a -> Ptr b -> IO ()
oneFrame opts bus m fbBuf audioBuf = do
    frameCycles <- Bus.cpuMCyclesPerLcdFrame bus
    -- The slack matches the frontend: a safety margin for LCD-off periods.
    _ <- runUntilFrame (frameCycles + 32) m
    case optMode opts of
        ModeNoBlit -> pure ()
        ModeFull -> do
            Bus.copyFramebufferRgbWithPitch
                (castPtr fbBuf)
                (Ppu.framebufferWidth * 3)
                bus
            n <- Bus.drainAudioSamplesInto (castPtr audioBuf :: Ptr Int16) audioCapacity bus
            n `seq` pure ()

median :: [Double] -> Double
median [] = 0
median xs =
    let s = sort xs
        n = length s
     in if odd n
            then s !! (n `div` 2)
            else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2
