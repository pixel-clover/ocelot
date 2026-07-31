{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ocelot.TimerSpec (spec) where

import Data.Word (Word8)
import Ocelot.Timer
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

{- | Reference implementation of a multi-cycle advance: @n@ separate one-M-cycle
advances, OR-ing the overflow flags the way 'Ocelot.Bus.advance' does.
-}
advanceStepwise :: Int -> TimerState -> (TimerState, Bool)
advanceStepwise n ts0 = go n ts0 False
  where
    go 0 ts ov = (ts, ov)
    go k ts ov =
        let (ts', f) = advance 1 ts
         in go (k - 1) ts' (ov || f)

{- | M-cycle counts in the range a single instruction can consume. Large counts
would only make the stepwise reference slow without reaching new behaviour.
-}
mCycles :: Gen Int
mCycles = choose (0, 40)

instance Arbitrary TimerState where
    arbitrary = do
        divider <- arbitrary
        tima <- arbitrary
        tma <- arbitrary
        tac <- arbitrary
        prevAnd <- arbitrary
        -- Both reload windows are 4 T-cycle countdowns. Generating arbitrary Ints
        -- here would only exercise states the timer can never be in.
        reloadCounter <- choose (0, 4)
        reloadedCounter <- choose (0, 4)
        pure (TimerState divider tima tma tac prevAnd reloadCounter reloadedCounter)

{- | State-only wrappers. 'writeDiv' and 'writeTac' report whether the edge they
produce overflowed TIMA, because the bus has to latch @IF@ for a write-driven
overflow straight away; these tests only care about the resulting state.
-}
writeDivS :: TimerState -> TimerState
writeDivS = fst . writeDiv

writeTacS :: Word8 -> TimerState -> TimerState
writeTacS v = fst . writeTac v

spec :: Spec
spec = do
    describe "DIV" $ do
        it "starts at 0 and exposes the upper 8 bits of the internal counter" $ do
            readDiv initialTimer `shouldBe` 0x00

        it "advancing 64 M-cycles ticks the divider by 256 T-cycles -> DIV becomes 1" $ do
            let (ts, _) = advance 64 initialTimer
            -- 64 M-cycles = 256 T-cycles; upper byte goes from 0 to 1.
            readDiv ts `shouldBe` 0x01

        it "any write to DIV resets the entire counter" $ do
            let (ts, _) = advance 1024 initialTimer
                ts' = writeDivS ts
            readDiv ts' `shouldBe` 0x00

    describe "TIMA when disabled" $ do
        it "does not increment without TAC bit 2 set" $ do
            let (ts, _) = advance 10000 initialTimer
            readTima ts `shouldBe` 0x00

    describe "TIMA increment rates" $ do
        it "TAC=0x05 (262144 Hz) increments TIMA every 16 T-cycles" $ do
            -- TAC=0x05 => bit 2 (enable) + bits 0..1 = 01 (16 T-cycle period).
            let ts0 = writeTacS 0x05 initialTimer
                -- Advance 4 M-cycles = 16 T-cycles -> one TIMA tick.
                (ts1, ov) = advance 4 ts0
            readTima ts1 `shouldBe` 0x01
            ov `shouldBe` False

        it "TAC=0x04 (4096 Hz) increments TIMA every 1024 T-cycles" $ do
            let ts0 = writeTacS 0x04 initialTimer
                -- 256 M-cycles = 1024 T-cycles.
                (ts1, _) = advance 256 ts0
            readTima ts1 `shouldBe` 0x01

    describe "TIMA overflow" $ do
        it "wraps to 0 immediately, then reloads from TMA one M-cycle later" $ do
            let ts0 =
                    writeTacS
                        0x05 -- Enabled, fast rate
                        ( writeTma
                            0x42 -- TMA = 0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- 4 M-cycles = 16 T-cycles: hits the falling edge that wraps TIMA from 0xFF to 0x00,
                -- but the reload window has not yet expired, so TIMA reads as 0 and IF has not fired.
                (ts1, ov1) = advance 4 ts0
                -- 1 more M-cycle (4 T-cycles) completes the reload window: TIMA := TMA, IF fires.
                (ts2, ov2) = advance 1 ts1
            readTima ts1 `shouldBe` 0x00
            ov1 `shouldBe` False
            readTima ts2 `shouldBe` 0x42
            ov2 `shouldBe` True

        it "does not signal overflow when no overflow occurred" $ do
            let ts0 = writeTacS 0x05 initialTimer
                (_, ov) = advance 4 ts0
            ov `shouldBe` False

        it "writing TIMA during the reload window cancels the reload and the IF" $ do
            let ts0 =
                    writeTacS
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- Land in the reload window (TIMA wrapped to 0, counter=4).
                (ts1, _) = advance 4 ts0
                -- Sneak in a TIMA write before the reload fires.
                ts2 = writeTima 0x99 ts1
                -- Drain the reload window. With the cancel in effect, no IF and TIMA stays at 0x99.
                (ts3, ov) = advance 1 ts2
            readTima ts3 `shouldBe` 0x99
            ov `shouldBe` False

        it "writing TMA during the reload window changes the loaded value" $ do
            let ts0 =
                    writeTacS
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                (ts1, _) = advance 4 ts0
                ts2 = writeTma 0x77 ts1 -- New TMA before reload fires
                (ts3, ov) = advance 1 ts2
            readTima ts3 `shouldBe` 0x77
            ov `shouldBe` True

        it "writing TMA during the reload window leaves TIMA reading 0 until the reload fires" $ do
            -- The reload window (T1..T3 after the wrap) must keep TIMA reading
            -- 0; only the *reloaded* window propagates a TMA write straight
            -- into TIMA. 'writeTma' used to set TIMA on both, so a read inside
            -- the reload window returned TMA instead of 0.
            let ts0 =
                    writeTacS
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                (ts1, _) = advance 4 ts0
                ts2 = writeTma 0x77 ts1
            readTima ts2 `shouldBe` 0x00

        it "writing TMA during the reloaded window propagates straight into TIMA" $ do
            let ts0 =
                    writeTacS
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- 5 M-cycles: the reload has fired (TIMA = TMA = 0x42) and we
                -- are inside the 4 T-cycle "reloaded" window.
                (ts1, ov) = advance 5 ts0
                ts2 = writeTma 0x77 ts1
            ov `shouldBe` True
            readTima ts1 `shouldBe` 0x42
            readTima ts2 `shouldBe` 0x77

        it "writing DIV that drops the AND signal high->low increments TIMA" $ do
            -- TAC=0x05 selects bit 3 of the divider. Pre-set divider so bit 3 is 1 (so the AND signal is high).
            -- Writing DIV resets to 0 and drops the AND signal, which is a falling edge.
            let ts0 = writeTacS 0x05 initialTimer
                -- Advance 8 T-cycles so divider's bit 3 becomes 1.
                (ts1, _) = advance 2 ts0
                -- Confirm TIMA hasn't ticked yet (bit 3 went 0->1, a rising edge, not a falling one).
                _ = readTima ts1 -- Still 0
                ts2 = writeDivS ts1
            readTima ts2 `shouldBe` 0x01

    describe "TAC masking" $ do
        it "writeTac stores only the low 3 bits" $ do
            let ts = writeTacS 0xFF initialTimer
            -- Low 3 bits set; readTac OR's in the unused-upper-bits-as-1 mask.
            readTac ts `shouldBe` 0xFF
        it "readTac reads back unused bits as 1 even when written as 0" $ do
            let ts = writeTacS 0x00 initialTimer
            readTac ts `shouldBe` 0xF8

    {- 'advance' is on the per-instruction hot path, so it is a standing candidate for
    being rewritten to compute the divider in closed form instead of stepping T-cycle
    by T-cycle. These properties are the guard rail for that: they pin batching to the
    one-cycle-at-a-time reference, which is the behaviour every TIMA edge test in this
    file implicitly assumes. 'Ocelot.ApuSpec' guards the APU the same way. -}
    describe "advance batching" $ do
        prop "advancing n M-cycles equals n separate one-cycle advances" $
            \ts -> forAll mCycles $ \n ->
                advance n ts === advanceStepwise n ts

        prop "advancing is additive across any split" $
            \ts -> forAll mCycles $ \a -> forAll mCycles $ \b ->
                let (tsA, ovA) = advance a ts
                    (tsAB, ovB) = advance b tsA
                 in advance (a + b) ts === (tsAB, ovA || ovB)

        prop "advancing zero cycles changes nothing and reports no overflow" $
            \ts -> advance 0 ts === (ts, False)

        prop "the divider gains exactly four T-cycles per M-cycle" $
            \ts -> forAll mCycles $ \n ->
                timDivider (fst (advance n ts))
                    === timDivider ts + fromIntegral (4 * n)

        prop "TMA and TAC are never modified by advancing" $
            \ts -> forAll mCycles $ \n ->
                let ts' = fst (advance n ts)
                 in (timTma ts', timTac ts') === (timTma ts, timTac ts)
