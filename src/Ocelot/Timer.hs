{-# LANGUAGE BangPatterns #-}

{- | Game Boy timer (DIV, TIMA, TMA, TAC).

The internal divider is a 16-bit counter ticked once per T-cycle. @DIV@
(@0xFF04@) reads the upper 8 bits; any write resets the entire counter to 0.

Real hardware does not increment TIMA at a fixed rate: it ANDs a specific
bit of the divider with the timer-enable bit (TAC bit 2), and TIMA
increments on the *falling edge* of that AND signal. The TAC rate bits
select which divider bit to use:

> 00 -> bit 9  (every 1024 T-cycles, 4096 Hz)
> 01 -> bit 3  (every 16 T-cycles, 262144 Hz)
> 10 -> bit 5  (every 64 T-cycles, 65536 Hz)
> 11 -> bit 7  (every 256 T-cycles, 16384 Hz)

The falling-edge view explains several "obscure timer" behaviors:

* Writing @DIV@ resets the divider to 0; if the AND signal was high
  (the selected bit was set and TAC enabled), the abrupt drop to 0
  produces a falling edge and TIMA increments once.
* Writing @TAC@ that changes the rate or clears the enable bit can
  similarly drive AND high to low, with the same effect.

TIMA overflow runs a small state machine across two M-cycle windows:

* T0 the falling edge wraps TIMA past @0xFF@; the wrap enters the
  /reloading/ window with TIMA reading as 0.
* T1..T3 TIMA still reads as 0; writes to TIMA here cancel the reload
  (and the IF), and writes to TMA queue the new reload value.
* T4 the reload fires: TIMA := TMA, @IF@ bit 2 set; TIMA enters the
  /reloaded/ window (one further M-cycle) where TIMA writes are ignored
  while TMA writes still propagate into TIMA.
* T8 TIMA returns to /running/.

This is the same three-state model SameBoy uses ('RUNNING' /
'RELOADING' / 'RELOADED'), required to match the mooneye
@tima_write_reloading@ and @tma_write_reloading@ acceptance ROMs.
-}
module Ocelot.Timer (
    TimerState (..),
    initialTimer,
    readDiv,
    readTima,
    readTma,
    readTac,
    writeDiv,
    writeTima,
    writeTma,
    writeTac,
    advance,
) where

import Data.Bits (shiftR, testBit, (.&.), (.|.))
import Data.Word (Word16, Word8)

data TimerState = TimerState
    { timDivider :: !Word16
    -- ^ Internal 16-bit T-cycle counter. @DIV@ exposes the upper 8 bits.
    , timTima :: !Word8
    , timTma :: !Word8
    , timTac :: !Word8
    , timPrevAnd :: !Bool
    -- ^ Previous AND of (timer-enabled) and (selected divider bit).
    -- TIMA increments on the falling edge of this signal.
    , timReloadCounter :: !Int
    -- ^ T-cycles remaining in the /reloading/ window; 0 means not in
    -- that window. Set to 4 when a falling edge wraps TIMA from 0xFF
    -- to 0x00. During the count-down TIMA reads as 0; a TIMA write
    -- cancels the reload (and the IF), and a TMA write changes the
    -- value the reload will load.
    , timReloadedCounter :: !Int
    -- ^ T-cycles remaining in the /reloaded/ window that follows a
    -- successful reload. While this is non-zero, TIMA writes are
    -- ignored (the just-loaded value is "frozen" for one M-cycle),
    -- but TMA writes still propagate into TIMA.
    }
    deriving (Eq, Show)

initialTimer :: TimerState
initialTimer = TimerState 0 0 0 0 False 0 0

readDiv :: TimerState -> Word8
readDiv ts = fromIntegral (timDivider ts `shiftR` 8)

readTima :: TimerState -> Word8
readTima = timTima

readTma :: TimerState -> Word8
readTma = timTma

readTac :: TimerState -> Word8
readTac ts = timTac ts .|. 0xF8 -- unused upper bits read as 1

----------------------------------------------------------------------
-- Edge-detector core
----------------------------------------------------------------------

-- | Bit of 'timDivider' selected by the low two bits of @TAC@.
selectedDivBit :: Word8 -> Int
selectedDivBit tac = case tac .&. 0x03 of
    0x00 -> 9
    0x01 -> 3
    0x02 -> 5
    _ -> 7

-- | The "AND" signal: timer-enabled AND the selected divider bit.
andSignal :: Word16 -> Word8 -> Bool
andSignal d tac = testBit tac 2 && testBit d (selectedDivBit tac)

{- | Apply a falling-edge transition: increment TIMA. If TIMA wraps from
0xFF to 0x00, schedule a TMA-reload to fire 4 T-cycles later. (Note that
TIMA itself reads as 0 immediately on overflow; only the @IF@ raise and
the TMA load are delayed.)
-}
applyFallingEdge :: TimerState -> TimerState
applyFallingEdge ts
    | timTima ts == 0xFF = ts{timTima = 0x00, timReloadCounter = 4}
    | otherwise = ts{timTima = timTima ts + 1}

{- | Step one T-cycle. Returns the new state and whether the TMA reload
fired this T-cycle (the bus turns that into an @IF@ bit-2 raise).
-}
stepT :: TimerState -> (TimerState, Bool)
stepT ts0 =
    -- 1a. Tick the post-reload "ignore" window. Writes to TIMA stay
    --     gated for the duration; we just count it down.
    let !ic = timReloadedCounter ts0
        ts0a
            | ic > 0 = ts0{timReloadedCounter = ic - 1}
            | otherwise = ts0
        -- 1b. Tick the reloading window. If it expires this cycle, the
        --     reload fires: TIMA := TMA, the IF bit pulses, and we
        --     enter the 4 T-cycle "reloaded" window.
        !rc = timReloadCounter ts0a
        (ts1, fired) =
            if rc > 0
                then
                    let !rc' = rc - 1
                     in if rc' == 0
                            then
                                ( ts0a
                                    { timReloadCounter = 0
                                    , timTima = timTma ts0a
                                    , timReloadedCounter = 4
                                    }
                                , True
                                )
                            else (ts0a{timReloadCounter = rc'}, False)
                else (ts0a, False)
        -- 2. Tick the divider.
        !d' = timDivider ts1 + 1
        ts2 = ts1{timDivider = d'}
        -- 3. Detect falling edge of the AND signal; increment TIMA if so.
        !newAnd = andSignal d' (timTac ts2)
        !falling = timPrevAnd ts2 && not newAnd
        ts3 = if falling then applyFallingEdge ts2 else ts2
        -- 4. Latch the new AND for next cycle's edge detector.
        !ts4 = ts3{timPrevAnd = newAnd}
     in (ts4, fired)

----------------------------------------------------------------------
-- Public surface
----------------------------------------------------------------------

writeDiv :: TimerState -> TimerState
writeDiv ts =
    -- Reset the divider; if the AND signal was high it drops to low,
    -- producing a falling edge that increments TIMA once.
    let !ts1 = ts{timDivider = 0}
        !newAnd = andSignal 0 (timTac ts1)
        !ts2 = if timPrevAnd ts1 && not newAnd then applyFallingEdge ts1 else ts1
     in ts2{timPrevAnd = newAnd}

writeTac :: Word8 -> TimerState -> TimerState
writeTac v ts =
    -- Same edge detector: changing TAC can drive AND high -> low.
    let !ts1 = ts{timTac = v .&. 0x07}
        !newAnd = andSignal (timDivider ts1) (timTac ts1)
        !ts2 = if timPrevAnd ts1 && not newAnd then applyFallingEdge ts1 else ts1
     in ts2{timPrevAnd = newAnd}

writeTima :: Word8 -> TimerState -> TimerState
writeTima v ts
    -- During the /reloading/ window: write cancels the pending reload
    -- (and the @IF@ raise), TIMA takes the new value.
    | timReloadCounter ts > 0 = ts{timTima = v, timReloadCounter = 0}
    -- During the /reloaded/ window: TIMA was just loaded with TMA and
    -- writes are ignored for one M-cycle.
    | timReloadedCounter ts > 0 = ts
    | otherwise = ts{timTima = v}

writeTma :: Word8 -> TimerState -> TimerState
writeTma v ts =
    -- TMA always latches. While TIMA is in the /reloading/ window the
    -- pending reload will pick up the new value at fire time; while
    -- TIMA is in the /reloaded/ window the just-loaded value is
    -- replaced by the new TMA, matching the documented "TMA write
    -- during reload" behavior.
    let ts' = ts{timTma = v}
     in if timReloadCounter ts' > 0 || timReloadedCounter ts' > 0
            then ts'{timTima = v}
            else ts'

{- | Advance the timer by @mCycles@ M-cycles (@mCycles * 4@ T-cycles).
Returns the new state and whether the timer interrupt should be raised
at least once during this advance.
-}
advance :: Int -> TimerState -> (TimerState, Bool)
advance mCycles ts0 = go (mCycles * 4) ts0 False
  where
    go 0 !ts !ov = (ts, ov)
    go !n !ts !ov =
        let (!ts', !f) = stepT ts
         in go (n - 1) ts' (ov || f)
