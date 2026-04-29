{-# LANGUAGE BangPatterns #-}

{- | Game Boy timer (DIV, TIMA, TMA, TAC).

The internal divider is a 16-bit counter ticked once per T-cycle. @DIV@
(@0xFF04@) reads the upper 8 bits; any write resets the entire counter to 0.

@TIMA@ increments at one of four rates selected by the low two bits of @TAC@,
when the timer is enabled (bit 2 of @TAC@):

> 00 -> 1024 T-cycles  (4096 Hz)
> 01 ->   16 T-cycles  (262144 Hz)
> 10 ->   64 T-cycles  (65536 Hz)
> 11 ->  256 T-cycles  (16384 Hz)

When @TIMA@ overflows from @0xFF@ to (would-be) @0x100@ it reloads from @TMA@
and signals the bus to set the Timer bit (bit 2) of @IF@. The
single-cycle "0x00 then TMA" overflow window and the @TAC@ falling-edge
glitch are not modeled; the simpler "increment by accumulated rate" rule is
correct for the vast majority of ROMs and matches blargg's @cpu_instrs@
@02-interrupts@ expectations.
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
    , timTimaAccum :: !Word16
    -- ^ T-cycles accumulated toward the next TIMA tick.
    , timTima :: !Word8
    , timTma :: !Word8
    , timTac :: !Word8
    }
    deriving (Eq, Show)

initialTimer :: TimerState
initialTimer = TimerState 0 0 0 0 0

readDiv :: TimerState -> Word8
readDiv ts = fromIntegral (timDivider ts `shiftR` 8)

readTima :: TimerState -> Word8
readTima = timTima

readTma :: TimerState -> Word8
readTma = timTma

readTac :: TimerState -> Word8
readTac ts = timTac ts .|. 0xF8 -- unused upper bits read as 1

writeDiv :: TimerState -> TimerState
writeDiv ts = ts{timDivider = 0, timTimaAccum = 0}

writeTima :: Word8 -> TimerState -> TimerState
writeTima v ts = ts{timTima = v}

writeTma :: Word8 -> TimerState -> TimerState
writeTma v ts = ts{timTma = v}

writeTac :: Word8 -> TimerState -> TimerState
writeTac v ts = ts{timTac = v .&. 0x07}

{- | Advance the timer by @mCycles@ M-cycles (@mCycles * 4@ T-cycles). Returns
the new state and whether @TIMA@ overflowed at least once during this advance
(in which case the bus must set bit 2 of @IF@).
-}
advance :: Int -> TimerState -> (TimerState, Bool)
advance mCycles ts =
    let !t = fromIntegral (mCycles * 4) :: Word16
        !ts1 = ts{timDivider = timDivider ts + t}
     in if not (timerEnabled ts1)
            then (ts1, False)
            else
                let !rate = rateFor (timTac ts1)
                    (accum', tima', overflowed) =
                        tickTima rate (timTimaAccum ts1 + t) (timTima ts1) (timTma ts1)
                 in (ts1{timTimaAccum = accum', timTima = tima'}, overflowed)

timerEnabled :: TimerState -> Bool
timerEnabled ts = testBit (timTac ts) 2

rateFor :: Word8 -> Word16
rateFor tac = case tac .&. 0x03 of
    0x00 -> 1024
    0x01 -> 16
    0x02 -> 64
    _ -> 256 -- 0x03

tickTima :: Word16 -> Word16 -> Word8 -> Word8 -> (Word16, Word8, Bool)
tickTima rate accum0 tima0 tma = go accum0 tima0 False
  where
    go !a !t !ov
        | a < rate = (a, t, ov)
        | t == 0xFF = go (a - rate) tma True
        | otherwise = go (a - rate) (t + 1) ov
