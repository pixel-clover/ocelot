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
        it "wraps to TMA and signals overflow" $ do
            let ts0 =
                    writeTac
                        0x05 -- enabled, fast rate
                        ( writeTma
                            0x42 -- TMA = 0x42
                            (writeTima 0xFF initialTimer)
                        )
                -- One tick from 0xFF should overflow.
                (ts1, ov) = advance 4 ts0
            ov `shouldBe` True
            readTima ts1 `shouldBe` 0x42

        it "does not signal overflow when no overflow occurred" $ do
            let ts0 = writeTac 0x05 initialTimer
                (_, ov) = advance 4 ts0
            ov `shouldBe` False

    describe "TAC masking" $ do
        it "writeTac stores only the low 3 bits" $ do
            let ts = writeTac 0xFF initialTimer
            -- Low 3 bits set; readTac OR's in the unused-upper-bits-as-1 mask.
            readTac ts `shouldBe` 0xFF
        it "readTac reads back unused bits as 1 even when written as 0" $ do
            let ts = writeTac 0x00 initialTimer
            readTac ts `shouldBe` 0xF8
