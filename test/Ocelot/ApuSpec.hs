{-# LANGUAGE OverloadedStrings #-}

module Ocelot.ApuSpec (spec) where

import Data.Bits ((.&.))
import Ocelot.Apu
import Test.Hspec

spec :: Spec
spec = do
    describe "default state" $ do
        it "NR52 reports power on, all channels off, with unused bits set" $ do
            apu <- initial
            v <- read8 0xFF26 apu
            (v .&. 0x80) `shouldBe` 0x80 -- power on
            (v .&. 0x70) `shouldBe` 0x70 -- unused bits read 1
            (v .&. 0x0F) `shouldBe` 0x00 -- no channel enabled
    describe "register read masks" $ do
        it "NR10 high bit reads as 1" $ do
            apu <- initial
            v <- read8 0xFF10 apu
            (v .&. 0x80) `shouldBe` 0x80

        it "NR13 (write-only) reads 0xFF" $ do
            apu <- initial
            v <- read8 0xFF13 apu
            v `shouldBe` 0xFF

        it "NR14 reports length-enable in bit 6 with the rest 0xBF-masked" $ do
            apu <- initial
            v <- read8 0xFF14 apu
            (v .&. 0xBF) `shouldBe` 0xBF

    describe "channel 2 trigger" $ do
        it "writing the trigger bit and a non-zero envelope enables ch2 in NR52" $ do
            apu <- initial
            -- NR21: 50% duty, no length restriction.
            write8 0xFF16 0x80 apu
            -- NR22: initial volume 15, envelope down, period 0.
            write8 0xFF17 0xF0 apu
            -- NR23: freq low byte
            write8 0xFF18 0x00 apu
            -- NR24: trigger + freq high (0)
            write8 0xFF19 0x80 apu
            v <- read8 0xFF26 apu
            (v .&. 0x02) `shouldBe` 0x02

        it "DAC off (NR22 = 0) disables the channel even after trigger" $ do
            apu <- initial
            write8 0xFF17 0x00 apu -- DAC off
            write8 0xFF19 0x80 apu -- trigger
            v <- read8 0xFF26 apu
            (v .&. 0x02) `shouldBe` 0x00

    describe "wave RAM" $ do
        it "round-trips a byte at 0xFF30" $ do
            apu <- initial
            write8 0xFF30 0xAB apu
            v <- read8 0xFF30 apu
            v `shouldBe` 0xAB

        it "round-trips bytes across the whole 16-byte wave RAM" $ do
            apu <- initial
            mapM_ (\i -> write8 (0xFF30 + fromIntegral i) (fromIntegral (i + 1)) apu) [0 .. 15 :: Int]
            vs <- mapM (\i -> read8 (0xFF30 + fromIntegral i) apu) [0 .. 15 :: Int]
            vs `shouldBe` [1 .. 16]

    describe "NR52 power-off" $ do
        it "powering off zeros NR50 and disables all channels" $ do
            apu <- initial
            -- Trigger ch2.
            write8 0xFF17 0xF0 apu
            write8 0xFF19 0x80 apu
            -- Set NR50 to a known non-zero value.
            write8 0xFF24 0x77 apu
            -- Power off.
            write8 0xFF26 0x00 apu
            nr50 <- read8 0xFF24 apu
            nr52 <- read8 0xFF26 apu
            nr50 `shouldBe` 0x00
            (nr52 .&. 0x80) `shouldBe` 0x00 -- power off
            (nr52 .&. 0x0F) `shouldBe` 0x00 -- no channels enabled
    describe "advance produces samples" $ do
        it "after triggering ch2 and advancing 1 frame, samples are emitted" $ do
            apu <- initial
            -- Set up a 1 kHz square wave: freq = 2048 - 4194304/(32*1000) = 1917.
            -- Encode: low byte = 1917 & 0xFF, high bits = (1917 >> 8) & 7.
            write8 0xFF24 0x77 apu -- master vol both sides 7
            write8 0xFF25 0x22 apu -- pan ch2 to both sides
            write8 0xFF16 0x80 apu -- 50% duty
            write8 0xFF17 0xF0 apu -- vol 15, env down period 0
            write8 0xFF18 0x7D apu -- freq low (0x77D = 1917)
            write8 0xFF19 0x87 apu -- trigger + freq high
            advance 17556 apu -- one frame
            samples <- drainSamples apu
            length samples `shouldSatisfy` (> 0)
            -- At least one of the samples should be non-zero (the channel is producing output).
            any (/= 0) samples `shouldBe` True
