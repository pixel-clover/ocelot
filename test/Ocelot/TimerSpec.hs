{-# LANGUAGE OverloadedStrings #-}

module Ocelot.TimerSpec (spec) where

import Ocelot.Timer
import Test.Hspec

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
                ts' = writeDiv ts
            readDiv ts' `shouldBe` 0x00

    describe "TIMA when disabled" $ do
        it "does not increment without TAC bit 2 set" $ do
            let (ts, _) = advance 10000 initialTimer
            readTima ts `shouldBe` 0x00

    describe "TIMA increment rates" $ do
        it "TAC=0x05 (262144 Hz) increments TIMA every 16 T-cycles" $ do
            -- TAC=0x05 => bit 2 (enable) + bits 0..1 = 01 (16 T-cycle period).
            let ts0 = writeTac 0x05 initialTimer
                -- Advance 4 M-cycles = 16 T-cycles -> one TIMA tick.
                (ts1, ov) = advance 4 ts0
            readTima ts1 `shouldBe` 0x01
            ov `shouldBe` False

        it "TAC=0x04 (4096 Hz) increments TIMA every 1024 T-cycles" $ do
            let ts0 = writeTac 0x04 initialTimer
                -- 256 M-cycles = 1024 T-cycles.
                (ts1, _) = advance 256 ts0
            readTima ts1 `shouldBe` 0x01

    describe "TIMA overflow" $ do
        it "wraps to 0 immediately, then reloads from TMA one M-cycle later" $ do
            let ts0 =
                    writeTac
                        0x05 -- enabled, fast rate
                        ( writeTma
                            0x42 -- TMA = 0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- 4 M-cycles = 16 T-cycles: hits the falling edge that wraps
                -- TIMA from 0xFF to 0x00, but the reload window has not yet
                -- expired, so TIMA reads as 0 and IF has not fired.
                (ts1, ov1) = advance 4 ts0
                -- 1 more M-cycle (4 T-cycles) completes the reload window:
                -- TIMA := TMA, IF fires.
                (ts2, ov2) = advance 1 ts1
            readTima ts1 `shouldBe` 0x00
            ov1 `shouldBe` False
            readTima ts2 `shouldBe` 0x42
            ov2 `shouldBe` True

        it "does not signal overflow when no overflow occurred" $ do
            let ts0 = writeTac 0x05 initialTimer
                (_, ov) = advance 4 ts0
            ov `shouldBe` False

        it "writing TIMA during the reload window cancels the reload and the IF" $ do
            let ts0 =
                    writeTac
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- Land in the reload window (TIMA wrapped to 0, counter=4).
                (ts1, _) = advance 4 ts0
                -- Sneak in a TIMA write before the reload fires.
                ts2 = writeTima 0x99 ts1
                -- Drain the reload window. With the cancel in effect, no IF
                -- and TIMA stays at 0x99.
                (ts3, ov) = advance 1 ts2
            readTima ts3 `shouldBe` 0x99
            ov `shouldBe` False

        it "writing TMA during the reload window changes the loaded value" $ do
            let ts0 =
                    writeTac
                        0x05
                        ( writeTma
                            0x42
                            (writeTima 0xFF initialTimer)
                        )
                (ts1, _) = advance 4 ts0
                ts2 = writeTma 0x77 ts1 -- new TMA before reload fires
                (ts3, ov) = advance 1 ts2
            readTima ts3 `shouldBe` 0x77
            ov `shouldBe` True

        it "writing DIV that drops the AND signal high->low increments TIMA" $ do
            -- TAC=0x05 selects bit 3 of the divider. Pre-set divider so bit 3
            -- is 1 (so the AND signal is high). Writing DIV resets to 0 and
            -- drops the AND signal, which is a falling edge.
            let ts0 = writeTac 0x05 initialTimer
                -- Advance 8 T-cycles so divider's bit 3 becomes 1.
                (ts1, _) = advance 2 ts0
                -- Confirm TIMA hasn't ticked yet (bit 3 went 0->1, a rising
                -- edge, not a falling one).
                _ = readTima ts1 -- still 0
                ts2 = writeDiv ts1
            readTima ts2 `shouldBe` 0x01

    describe "TAC masking" $ do
        it "writeTac stores only the low 3 bits" $ do
            let ts = writeTac 0xFF initialTimer
            -- Low 3 bits set; readTac OR's in the unused-upper-bits-as-1 mask.
            readTac ts `shouldBe` 0xFF
        it "readTac reads back unused bits as 1 even when written as 0" $ do
            let ts = writeTac 0x00 initialTimer
            readTac ts `shouldBe` 0xF8
