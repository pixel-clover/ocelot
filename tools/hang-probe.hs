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
import Control.Monad (when)
import Data.Bits (shiftR, testBit, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runUntilFrame, step)
import Ocelot.Cpu.Registers (regPC, regSP)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Joypad (Button (..))
import Ocelot.Machine (Machine (..), debugSummary, machineFromCartridge)
import qualified Ocelot.Snapshot as Snapshot
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

{- | Randomized multi-button input for the fuzz mode.

'buttonAt' presses one button at a time on a fixed rota, which walks through menus but
plays nothing like a person: real play holds Right while tapping A and B, and that is
the input space where a mid-game crash lives. This draws a button set per 8-frame slot
from a splitmix64 hash of (seed, slot), biased toward the hold-Right-and-jump shape of
a side-scroller, with occasional Start presses because pause and unpause exercise
interrupt timing that steady play never touches.
-}
fuzzButtonsAt :: Int -> Int -> [Button]
fuzzButtonsAt seed frame
    | frame < 0 = []
    | otherwise = dpad <> aBtn <> bBtn <> start <> select
  where
    slot = frame `div` 8
    h = splitmix64 (fromIntegral seed * 0x9E3779B97F4A7C15 + fromIntegral slot)
    -- Cases 6 and 7 are combinations a physical D-pad cannot produce. A browser
    -- keyboard delivers them (ArrowLeft and ArrowRight are independent keys), and
    -- games written against real pads never see them, so they probe input handling
    -- no play testing on hardware ever exercised.
    dpad = case h .&. 0x7 :: Word64 of
        0 -> [ButtonRight]
        1 -> [ButtonRight]
        2 -> [ButtonRight]
        3 -> [ButtonRight]
        4 -> [ButtonLeft]
        5 -> [ButtonUp]
        6 -> [ButtonLeft, ButtonRight]
        _ -> [ButtonUp, ButtonDown]
    aBtn = [ButtonA | testBit h 3]
    bBtn = [ButtonB | testBit h 4 && testBit h 5]
    start = [ButtonStart | (h `shiftR` 6) .&. 0x3F == 0]
    select = [ButtonSelect | (h `shiftR` 12) .&. 0xFF == 0]

splitmix64 :: Word64 -> Word64
splitmix64 x0 =
    let x1 = (x0 `xor` (x0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
        x2 = (x1 `xor` (x1 `shiftR` 27)) * 0x94D049BB133111EB
     in x2 `xor` (x2 `shiftR` 31)

-- | The frame's input as a set, in either generated-input mode.
inputAt :: Bool -> Int -> Int -> [Button]
inputAt fuzz seed frame
    | fuzz = fuzzButtonsAt seed frame
    | otherwise = maybeToList (buttonAt seed frame)

{- | Press this frame's buttons, releasing only what is no longer held.

Releasing everything and re-pressing would put a same-frame release-press edge on a
held button, which no physical pad produces.
-}
applyInput :: Machine -> Bool -> Int -> Int -> IO ()
applyInput m fuzz seed frame = do
    let prev = inputAt fuzz seed (frame - 1)
        cur = inputAt fuzz seed frame
    mapM_ (\btn -> Bus.setButton btn False (machineBus m)) (filter (`notElem` cur) prev)
    mapM_ (\btn -> Bus.setButton btn True (machineBus m)) cur

{- | A recorded input log from the web frontend's freeze report.

One event per line, @frame button down@: the frame offset relative to the pre-freeze
state, the wasm button code, and 1 for press or 0 for release. The web Worker records
every button event with the count of frames fully run, and this replays each event
before the same frame runs, which is when the browser's message loop delivered it.
-}
parseInputLog :: String -> M.Map Int [(Button, Bool)]
parseInputLog text =
    M.fromListWith
        (flip (++))
        [ (frame, [(btn, down /= (0 :: Int))])
        | l <- lines text
        , not (null l)
        , head l /= '#'
        , [frameStr, codeStr, downStr] <- [words l]
        , (frame, "") <- reads frameStr
        , (code, "") <- reads codeStr
        , (down, "") <- reads downStr
        , Just btn <- [buttonFromCode code]
        ]

-- | The wasm export button numbering (see 'normalizeButton' in app-web/Main.hs).
buttonFromCode :: Int -> Maybe Button
buttonFromCode 0 = Just ButtonUp
buttonFromCode 1 = Just ButtonDown
buttonFromCode 2 = Just ButtonLeft
buttonFromCode 3 = Just ButtonRight
buttonFromCode 4 = Just ButtonA
buttonFromCode 5 = Just ButtonB
buttonFromCode 6 = Just ButtonStart
buttonFromCode 7 = Just ButtonSelect
buttonFromCode _ = Nothing

applyReplayInput :: Machine -> M.Map Int [(Button, Bool)] -> Int -> IO ()
applyReplayInput m log frame =
    mapM_
        (\(btn, down) -> Bus.setButton btn down (machineBus m))
        (M.findWithDefault [] frame log)

data WatchMode = NoWatch | WatchReset | WatchRunaway

main :: IO ()
main = do
    args <- getArgs
    let parseFlags mode fuzz sp rp ("--watch-reset" : more) = parseFlags WatchReset fuzz sp rp more
        parseFlags mode fuzz sp rp ("--watch-runaway" : more) = parseFlags WatchRunaway fuzz sp rp more
        parseFlags mode _ sp rp ("--fuzz" : more) = parseFlags mode True sp rp more
        parseFlags mode fuzz _ rp ("--state" : sp : more) = parseFlags mode fuzz (Just sp) rp more
        parseFlags mode fuzz sp _ ("--replay" : rp : more) = parseFlags mode fuzz sp (Just rp) more
        parseFlags mode fuzz sp rp more = (mode, fuzz, sp, rp, more)
        (watchMode, fuzz, statePath, replayPath, rest) = parseFlags NoWatch False Nothing Nothing args
    (path, frames, seed) <- case rest of
        [p] -> pure (p, 3600 :: Int, 0 :: Int)
        [p, n] -> pure (p, read n, 0)
        [p, n, sd] -> pure (p, read n, read sd)
        _ ->
            putStrLn "usage: hang-probe [--watch-reset|--watch-runaway] [--fuzz] [--state FILE] [--replay FILE] <rom> [frames] [input-seed]"
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
            inputFn <- case replayPath of
                Just rp -> do
                    recorded <- parseInputLog <$> readFile rp
                    printf "replaying %d input events from %s\n" (sum (map length (M.elems recorded))) rp
                    pure (applyReplayInput m recorded)
                Nothing -> pure (applyInput m fuzz seed)
            -- Boot legitimately passes through the entry point, so traps skip the first
            -- frames; resuming from a mid-game state has no boot to skip.
            let settle = case statePath of
                    Just _ -> 0
                    Nothing -> settleFrames
            case watchMode of
                NoWatch -> run m frames seed inputFn
                WatchReset -> watchFor resetTrap settle m frames seed inputFn
                WatchRunaway -> watchFor runawayTrap settle m frames seed inputFn

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

-- | The game restarted: nothing during play jumps to the cart entry point.
resetTrap :: Word16 -> Word16 -> Maybe String
resetTrap pc _sp
    | pc == 0x0100 = Just "cart entry point, i.e. the game restarted"
    | otherwise = Nothing

{- | Execution state no working game reaches, caught within a few instructions of the
corruption instead of seconds later when the picture freezes.

The stack in the ROM region is definitive: pushes there hit MBC registers and pops read
ROM bytes, so no game does it on purpose. PC in VRAM, cartridge RAM this cart does not
have, or the OAM/IO window is the CPU executing data. WRAM and HRAM are deliberately
not trapped, because games legitimately run code from both.
-}
runawayTrap :: Word16 -> Word16 -> Maybe String
runawayTrap pc sp
    | sp >= 0x0100 && sp < 0x8000 = Just "stack pointer inside the ROM region"
    | pc >= 0x8000 && pc <= 0xBFFF = Just "executing VRAM or cartridge RAM"
    | pc >= 0xFE00 && pc < 0xFF80 = Just "executing OAM or the IO window"
    | otherwise = Nothing

watchFor :: (Word16 -> Word16 -> Maybe String) -> Int -> Machine -> Int -> Int -> (Int -> IO ()) -> IO ()
watchFor trap settle m frames seed inputFn = do
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
            | frame >= frames = putStrLn "no trap fired"
            | otherwise = do
                inputFn frame
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
                    sp = regSP (cpuRegs cpu)
                    used' = used + fromIntegral (cpuCycles cpu - c0)
                remember pc
                case if frame > settle then trap pc sp else Nothing of
                    Just reason -> do
                        printf "TRAP at frame %d: pc=%04X sp=%04X (%s)\n" frame pc sp reason
                        pcs <- recent
                        putStrLn ("  last " <> show (length pcs) <> " PCs, oldest first:")
                        putStrLn ("    " <> unwords (map (printf "%04X") pcs))
                        reportState m
                        pure True
                    Nothing -> do
                        ready <- Bus.takeFrameReady (machineBus m)
                        if ready then pure False else instrLoop frame used' cap
    frameLoop 0

run :: Machine -> Int -> Int -> (Int -> IO ()) -> IO ()
run m frames seed inputFn = do
    lastHash <- newIORef (0 :: Word32)
    sameFor <- newIORef (0 :: Int)
    stallAt <- newIORef (Nothing :: Maybe Int)
    pcs <- newIORef (M.empty :: M.Map Word16 Int)
    capped <- newIORef (0 :: Int)
    let go !i
            | i >= frames = pure Nothing
            | otherwise = do
                inputFn i
                cap <- (+ 32) <$> Bus.cpuMCyclesPerLcdFrame (machineBus m)
                r <- try (runUntilFrame cap m) :: IO (Either SomeException Int)
                case r of
                    Left err -> pure (Just (i, displayException err))
                    Right used -> do
                        -- Hitting the cap means no VBlank edge arrived this frame.
                        when (used >= cap) (modifyCount capped)
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
                                when (n' >= stallFrames) (writeIORef stallAt (Just (i - n')) >> samplePc)
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
                    mapM_ (uncurry (printf "    pc=%04X  %d\n")) top
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
