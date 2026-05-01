{-# LANGUAGE OverloadedStrings #-}

module Ocelot.ApuSpec (spec) where

import Data.Bits ((.&.), (.|.))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word16, Word8)
import qualified Numeric
import Ocelot.Apu
import Test.Hspec

spec :: Spec
spec = do
    describe "default state" $ do
        it "NR52 reports power off, all channels off, with unused bits set (hardware power-on)" $ do
            apu <- initial
            v <- read8 0xFF26 apu
            (v .&. 0x80) `shouldBe` 0x00 -- power off (boot ROM enables it)
            (v .&. 0x70) `shouldBe` 0x70 -- unused bits read 1
            (v .&. 0x0F) `shouldBe` 0x00 -- no channel enabled
        it "NR52 reports power on after writing 0x80 to NR52" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            v <- read8 0xFF26 apu
            (v .&. 0x80) `shouldBe` 0x80
            (v .&. 0x0F) `shouldBe` 0x00
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

        -- The blargg dmg_sound `01-registers` ROM verifies that after writing
        -- 0xFF to each NR register, the read returns the value OR'd with the
        -- register's "unused-bits-read-as-1" mask. We can't run that ROM
        -- end-to-end (it reports via the LCD, not serial), so lock the
        -- masks in here to catch regressions from a regression in
        -- 'readRegister'.
        mapM_ regMaskCase nrReadMasks

    describe "blargg test_rw walk" $ do
        -- Mirror dmg_sound 01-registers test 2: for every register from
        -- NR10..NR51 (skipping NR52) and every D in 0..255, write D, expect
        -- read == (D | mask). Between iterations the ROM mutes panning and
        -- disables the wave channel, so we do the same.
        it "every (addr, D) round-trips read = D | mask" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- power on
            failures <- collectRwFailures apu
            failures `shouldBe` []

    describe "sweep overflow on trigger" $ do
        it "trigger with shift>0 and shadow+shadow>>shift > 0x7FF disables ch1" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- power on
            write8 0xFF12 0x08 apu -- DAC on (bits 7..3 nonzero)
            -- NR10: sweep period=0 (timer treated as 8), no negate, shift=1.
            write8 0xFF10 0x01 apu
            -- Frequency = 0x7FF. On trigger: 0x7FF + (0x7FF >> 1) = 0xBFE
            -- which exceeds 11 bits, so ch1 is immediately disabled.
            write8 0xFF13 0xFF apu
            write8 0xFF14 0x87 apu
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x00

    describe "length-counter extra clock quirk" $ do
        -- After one frame-sequencer step (2048 M-cycles), the next step
        -- is step 1 which does not clock the length counter, putting us
        -- in the "first half" of a length period.
        it "len-en 0->1 in first half clocks the length counter" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            write8 0xFF12 0xF0 apu -- DAC on, vol 15
            write8 0xFF11 0x3E apu -- length = 64 - 62 = 2
            write8 0xFF14 0x80 apu -- trigger, len-en=0
            advance 2048 apu -- step into first half
            write8 0xFF14 0x40 apu -- enable length: clocks length 2->1
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x01 -- still on
            -- Two more frame-sequencer steps reach step 2 (next length
            -- tick), which clocks 1 -> 0 and disables the channel.
            advance 4096 apu
            v' <- read8 0xFF26 apu
            (v' .&. 0x01) `shouldBe` 0x00
        it "trigger with len-en already 1 reloads length=0 and clocks once in first half" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            write8 0xFF12 0xF0 apu -- DAC on
            write8 0xFF11 0x3F apu -- length = 64 - 63 = 1
            advance 2048 apu -- first half
            write8 0xFF14 0x40 apu -- enable length: clocks 1 -> 0, channel disabled
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x00
            -- Trigger with len-en still 1: trigger reloads length 0 -> 64,
            -- then the post-trigger extra clock decrements to 63.
            write8 0xFF14 0xC0 apu
            v' <- read8 0xFF26 apu
            (v' .&. 0x01) `shouldBe` 0x01 -- channel re-enabled
    describe "channel 2 trigger" $ do
        it "writing the trigger bit and a non-zero envelope enables ch2 in NR52" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- power APU on (post-boot handoff)
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
        it "while powered off, writes to NR12 (ch1 envelope) are ignored" $ do
            apu <- initial
            write8 0xFF26 0x00 apu -- power off
            write8 0xFF12 0xF0 apu -- attempt write while off
            -- After power-on, NR12 should still read as if cleared (not 0xF0).
            write8 0xFF26 0x80 apu -- power on
            v <- read8 0xFF12 apu
            v `shouldBe` 0x00

        it "DMG vs CGB: NRx1 length writes while powered off differ" $ do
            -- DMG accepts the length value; CGB ignores it. We probe by
            -- triggering ch1 with length-enable after the length write,
            -- then driving a few frame-sequencer length steps. The DMG
            -- write loaded length=1, so the channel disables on the very
            -- next length step; the CGB write was discarded so length
            -- defaults to 64 and the channel stays on. Matches SameBoy
            -- 'GB_apu_write' line 1685 (the @reg != GB_IO_NRx1@ guard
            -- only applies on DMG).
            let probe isCgb = do
                    apu <- initial
                    setCgbMode isCgb apu
                    write8 0xFF26 0x00 apu -- power off
                    write8 0xFF11 0x3F apu -- length = 1 (DMG only takes effect)
                    write8 0xFF26 0x80 apu -- power on
                    write8 0xFF12 0xF0 apu -- envelope volume 15
                    write8 0xFF14 0xC0 apu -- trigger + length-enable
                    -- Two length-counter steps (each 16384 T-cycles).
                    advance 8192 apu
                    advance 8192 apu
                    nr52 <- read8 0xFF26 apu
                    pure (nr52 .&. 0x01)
            dmgBit <- probe False
            cgbBit <- probe True
            dmgBit `shouldBe` 0x00 -- ch1 off on DMG (length expired)
            cgbBit `shouldBe` 0x01 -- ch1 still on on CGB (length write blocked)
        it "powering off then on leaves channels disabled" $ do
            apu <- initial
            write8 0xFF17 0xF0 apu
            write8 0xFF19 0x80 apu -- trigger ch2
            write8 0xFF26 0x00 apu -- power off
            write8 0xFF26 0x80 apu -- power on
            v <- read8 0xFF26 apu
            (v .&. 0x0F) `shouldBe` 0x00

    describe "advance produces samples" $ do
        it "after triggering ch2 and advancing 1 frame, samples are emitted" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- power APU on (post-boot handoff)
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

collectRwFailures :: ApuState -> IO [(Word16, Word8, Word8, Word8)]
collectRwFailures apu = do
    -- Outer loop = D values (matches blargg's outer ld d,0 / inc d / jr nz).
    -- Inner loop = walk addresses from NR10..WAVE+0x10, skipping NR52.
    let addrs = [a | a <- [0xFF10 .. 0xFF3F], a /= 0xFF26]
    fails <- mapM (`walk` addrs) [0 :: Word8 .. 255]
    pure (concat fails)
  where
    walk d addrs = do
        bad <-
            mapM
                ( \addr -> do
                    let mask = lookupMask addr
                    write8 addr d apu
                    v <- read8 addr apu
                    let expected = d .|. mask
                    write8 0xFF25 0x00 apu
                    write8 0xFF1A 0x00 apu
                    pure $
                        if v == expected
                            then Nothing
                            else Just (addr, d, expected, v)
                )
                addrs
        pure (catMaybes bad)
    lookupMask addr
        | addr >= 0xFF30 && addr <= 0xFF3F = 0x00 -- wave RAM
        | otherwise = fromMaybe 0xFF (lookup addr nrReadMasks)

regMaskCase :: (Word16, Word8) -> Spec
regMaskCase (addr, mask) =
    it ("0x" <> showHex addr <> " (mask 0x" <> showHex mask <> ") readback after write 0xFF") $ do
        apu <- initial
        write8 0xFF26 0x80 apu -- power on
        write8 addr 0xFF apu
        v <- read8 addr apu
        (v .|. mask) `shouldBe` 0xFF
  where
    showHex :: (Integral a) => a -> String
    showHex n = let s = Numeric.showHex (fromIntegral n :: Int) "" in s

nrReadMasks :: [(Word16, Word8)]
nrReadMasks =
    [ (0xFF10, 0x80) -- NR10: bit 7 = 1
    , (0xFF11, 0x3F) -- NR11: bits 5..0 = 1 (duty in 7..6)
    , (0xFF12, 0x00) -- NR12: full byte
    , (0xFF13, 0xFF) -- NR13: write-only
    , (0xFF14, 0xBF) -- NR14: only bit 6 readable
    , (0xFF15, 0xFF) -- gap
    , (0xFF16, 0x3F) -- NR21
    , (0xFF17, 0x00) -- NR22
    , (0xFF18, 0xFF) -- NR23 write-only
    , (0xFF19, 0xBF) -- NR24
    , (0xFF1A, 0x7F) -- NR30: only bit 7 readable
    , (0xFF1B, 0xFF) -- NR31 write-only
    , (0xFF1C, 0x9F) -- NR32: bits 6..5 readable
    , (0xFF1D, 0xFF) -- NR33 write-only
    , (0xFF1E, 0xBF) -- NR34
    , (0xFF1F, 0xFF) -- gap
    , (0xFF20, 0xFF) -- NR41 write-only
    , (0xFF21, 0x00) -- NR42
    , (0xFF22, 0x00) -- NR43
    , (0xFF23, 0xBF) -- NR44
    , (0xFF24, 0x00) -- NR50
    , (0xFF25, 0x00) -- NR51
    ]
