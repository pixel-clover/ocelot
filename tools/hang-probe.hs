{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Run a ROM frame by frame the way the web frontend does and report where it
stops making progress.

The frame loop here is deliberately identical to 'Ocelot.Web.runFrame':
@runUntilFrame (cpuMCyclesPerLcdFrame + 32)@. The difference is that this keeps the
'Machine' handle, so when the picture stops changing it can say what the CPU is doing.
'Ocelot.Web.WebSession' is opaque and gives no way to look inside.

> make tools
> bin/tools/hang-probe roms/Adventure\ Island.gb 3600

Reports, per run: any Haskell exception escaping the frame (the web build swallows
these into @ocelot_last_error@, so on the web they present as a frozen picture with no
other symptom), the first frame whose framebuffer then stayed identical for
@stallFrames@ frames, and a program-counter histogram over that window. A tight PC
histogram means the guest is wedged; a broad one means it is running but not drawing.

@runUntilFrame@ returning its full cap rather than stopping early means no VBlank edge
arrived that frame, which is normal while a game holds the LCD off and suspicious
otherwise, so the count of capped frames is reported too.
-}
module Main (main) where

import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits (xor)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word32, Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import qualified Ocelot.Snapshot as Snapshot
import Ocelot.Cpu.Execute (runUntilFrame, step)
import Ocelot.Joypad (Button (..))
import Ocelot.Cpu.Registers (regPC)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), debugSummary, machineFromCartridge)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

-- | Identical framebuffers for this many consecutive frames counts as stalled.
stallFrames :: Int
stallFrames = 240

{- | Deterministic input, so a run is reproducible and a stall can be re-reached.

Without this the probe reaches a title screen and stops there, and the static picture
that follows is indistinguishable from a hang: the first version of this tool reported
three games as stalled when all three were sitting in their normal per-frame VBlank sync
waiting for a button. Holding a button across several frames matters, because games poll
the joypad once per frame and drop anything shorter.

The sequence leans on Start and A to get through title screens and menus, then walks
right, which is enough to get a side-scroller into actual gameplay.
-}
buttonAt :: Int -> Int -> Maybe Button
buttonAt seed frame
    | frame < 0 = Nothing
    | frame `mod` period < holdFrames = Just (pick ((frame `div` period) + seed))
    | otherwise = Nothing
  where
    period = 24
    holdFrames = 10
    pick n = case n `mod` 8 of
        0 -> ButtonStart
        1 -> ButtonA
        2 -> ButtonRight
        3 -> ButtonA
        4 -> ButtonRight
        5 -> ButtonB
        6 -> ButtonDown
        _ -> ButtonRight

main :: IO ()
main = do
    args <- getArgs
    (watchReset, args1) <- case args of
        ("--watch-reset" : more) -> pure (True, more)
        more -> pure (False, more)
    (statePath, rest) <- case args1 of
        ("--state" : sp : more) -> pure (Just sp, more)
        more -> pure (Nothing, more)
    (path, frames, seed) <- case rest of
        [p] -> pure (p, 3600 :: Int, 0 :: Int)
        [p, n] -> pure (p, read n, 0)
        [p, n, sd] -> pure (p, read n, read sd)
        _ ->
            putStrLn "usage: hang-probe [--watch-reset] [--state FILE] <rom> [frames] [input-seed]"
                >> exitFailure
    hSetBuffering stdout LineBuffering
    bytes <- BS.readFile path
    loaded <- Cartridge.loadRom bytes
    case loaded of
        Left e -> putStrLn ("loadRom: " <> show e) >> exitFailure
        Right cart -> do
            m <- machineFromCartridge cart
            printf "rom=%s bytes=%d cgb=%s\n" path (BS.length bytes) (show (Bus.isCgb (machineBus m)))
            -- Loading a state captured just before a hang is the only way to reach one that
            -- needs real play to provoke; the scripted input below cannot get everywhere.
            case statePath of
                Nothing -> pure ()
                Just sp -> do
                    blob <- BS.readFile sp
                    loadedState <- Snapshot.load blob m
                    case loadedState of
                        Left err -> putStrLn ("snapshot load failed: " <> show err) >> exitFailure
                        Right () -> printf "resumed from state %s (%d bytes)\n" sp (BS.length blob)
            if watchReset then watchForReset m frames seed else run m frames seed

{- | Step instruction by instruction watching for the game restarting.

A game that freezes and then starts over has re-entered its own initialisation, so the
trap is the cart entry point at @0x0100@. Nothing during play jumps there; only boot and
a deliberate reset do.

Do not trap on the @RST@ vectors, which is the tempting choice. @0xFF@ decodes as
@RST 38h@, so a fetch that reads @0xFF@ lands at @0x0038@ and that really is the
signature of the CPU executing unmapped memory. But games use the low vectors as
compact one-byte calls, and Adventure Island uses several: @0x0000@ holds
@POP HL; RST 20h; LD H,D; LD L,E; JP (HL)@, a jump-table dispatcher, and @0x0010@ holds
a 16-bit add helper ending in @RET@. Trapping @0x0000@ here reports normal execution on
the first eligible frame. Check what a ROM actually stores at a vector before treating
it as garbage; @0xFF@-filled means unmapped, real instructions mean it is a routine.

What makes a trap actionable is the trail: the last 'trailLength' program counters show
which routine walked off, which a cycle count alone never tells you.

Boot legitimately passes through the entry point, so the first 'settleFrames' frames are
ignored.
-}
trailLength :: Int
trailLength = 48

settleFrames :: Int
settleFrames = 120

watchForReset :: Machine -> Int -> Int -> IO ()
watchForReset m frames seed = do
    trail <- MV.replicate trailLength 0
    slot <- newIORef (0 :: Int)
    let remember !pc = do
            i <- readIORef slot
            MV.write trail (i `mod` trailLength) pc
            writeIORef slot (i + 1)
        recent = do
            i <- readIORef slot
            let n = min i trailLength
                order = [(i - n + k) `mod` trailLength | k <- [0 .. n - 1]]
            mapM (MV.read trail) order
        frameLoop !frame
            | frame >= frames = putStrLn "no restart seen" >> pure ()
            | otherwise = do
                mapM_ (\btn -> Bus.setButton btn False (machineBus m)) (buttonAt seed (frame - 1))
                mapM_ (\btn -> Bus.setButton btn True (machineBus m)) (buttonAt seed frame)
                cap <- (+ 32) <$> Bus.cpuMCyclesPerLcdFrame (machineBus m)
                trapped <- instrLoop frame 0 cap
                if trapped then pure () else frameLoop (frame + 1)
        instrLoop !frame !used !cap
            | used >= cap = pure False
            | otherwise = do
                before <- readIORef (machineCpu m)
                let c0 = cpuCycles before
                step m
                cpu <- readIORef (machineCpu m)
                let pc = regPC (cpuRegs cpu)
                    used' = used + fromIntegral (cpuCycles cpu - c0)
                remember pc
                if frame > settleFrames && pc == 0x0100
                    then do
                        printf "TRAP at frame %d: pc=%04X (%s)\n" frame pc (trapName pc)
                        pcs <- recent
                        putStrLn ("  last " <> show (length pcs) <> " PCs, oldest first:")
                        putStrLn ("    " <> unwords (map (printf "%04X") pcs))
                        reportState m
                        pure True
                    else do
                        ready <- Bus.takeFrameReady (machineBus m)
                        if ready then pure False else instrLoop frame used' cap
    frameLoop 0

trapName :: Word16 -> String
trapName 0x0100 = "cart entry point, i.e. the game restarted"
trapName _ = "unexpected"

run :: Machine -> Int -> Int -> IO ()
run m frames seed = do
    lastHash <- newIORef (0 :: Word32)
    sameFor <- newIORef (0 :: Int)
    stallAt <- newIORef (Nothing :: Maybe Int)
    pcs <- newIORef (M.empty :: M.Map Word16 Int)
    capped <- newIORef (0 :: Int)
    let go !i
            | i >= frames = pure Nothing
            | otherwise = do
                -- Release last frame's button before pressing this frame's, so a held
                -- run of frames reads as one press rather than several overlapping ones.
                mapM_ (\btn -> Bus.setButton btn False (machineBus m)) (buttonAt seed (i - 1))
                mapM_ (\btn -> Bus.setButton btn True (machineBus m)) (buttonAt seed i)
                cap <- (+ 32) <$> Bus.cpuMCyclesPerLcdFrame (machineBus m)
                r <- try (runUntilFrame cap m) :: IO (Either SomeException Int)
                case r of
                    Left err -> pure (Just (i, displayException err))
                    Right used -> do
                        -- Hitting the cap means no VBlank edge arrived this frame.
                        if used >= cap then modifyCount capped else pure ()
                        fb <- Bus.framebufferRgbBytes (machineBus m)
                        let !h = fnv1a fb
                        prev <- readIORef lastHash
                        writeIORef lastHash h
                        n <- readIORef sameFor
                        let !n' = if h == prev then n + 1 else 0
                        writeIORef sameFor n'
                        -- Once stalled, accumulate PCs so the report can show the loop.
                        stalled <- readIORef stallAt
                        case stalled of
                            Just _ -> samplePc
                            Nothing ->
                                if n' >= stallFrames
                                    then writeIORef stallAt (Just (i - n')) >> samplePc
                                    else pure ()
                        go (i + 1)
        samplePc = do
            cpu <- readIORef (machineCpu m)
            let pc = regPC (cpuRegs cpu)
            modifyIORef' pcs (M.insertWith (+) pc 1)
        modifyCount ref = readIORef ref >>= \v -> writeIORef ref (v + 1)
        modifyIORef' ref f = readIORef ref >>= \v -> writeIORef ref $! f v
    outcome <- go 0
    nCapped <- readIORef capped
    printf "frames=%d seed=%d capped(no vblank edge)=%d\n" frames seed nCapped
    case outcome of
        Just (i, err) -> do
            printf "EXCEPTION at frame %d\n" i
            putStrLn ("  " <> err)
            putStrLn "  (the web build catches this and reports it via ocelot_last_error)"
        Nothing -> do
            stalled <- readIORef stallAt
            case stalled of
                Nothing -> putStrLn "no stall detected: the picture kept changing"
                Just at -> do
                    printf "STALLED from frame %d (framebuffer identical for >= %d frames)\n" at stallFrames
                    hist <- readIORef pcs
                    let top = take 12 (sortOn (negate . snd) (M.toList hist))
                        total = sum (M.elems hist)
                    printf "  distinct PCs sampled while stalled: %d over %d samples\n" (M.size hist) total
                    mapM_ (\(pc, c) -> printf "    pc=%04X  %d\n" pc c) top
            reportState m

{- | Machine state at the stall, via the same 'Ocelot.Machine.debugSummary' the web
watchdog reports, so a report from this tool and one pasted out of the browser console
have identical fields.
-}
reportState :: Machine -> IO ()
reportState m = do
    summary <- debugSummary m
    putStrLn ("  " <> BSC.unpack summary)

fnv1a :: BS.ByteString -> Word32
fnv1a = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 16777619) 2166136261
