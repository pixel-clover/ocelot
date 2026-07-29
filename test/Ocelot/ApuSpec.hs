{-# LANGUAGE OverloadedStrings #-}

module Ocelot.ApuSpec (spec) where

import Data.Bits ((.&.), (.|.))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)
import Foreign.Marshal.Array (allocaArray, peekArray)
import qualified Numeric
import Ocelot.Apu
import Test.Hspec

spec :: Spec
spec = do
    describe "default state" $ do
        it "NR52 reports power off, all channels off, with unused bits set (hardware power-on)" $ do
            apu <- initial
            v <- read8 0xFF26 apu
            (v .&. 0x80) `shouldBe` 0x00 -- Power off (boot ROM enables it)
            (v .&. 0x70) `shouldBe` 0x70 -- Unused bits read 1
            (v .&. 0x0F) `shouldBe` 0x00 -- No channel enabled
        it "NR52 reports power on after writing 0x80 to NR52" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            v <- read8 0xFF26 apu
            (v .&. 0x80) `shouldBe` 0x80
            (v .&. 0x0F) `shouldBe` 0x00
    describe "NR52 power-on frame sequencer" $ do
        {- Writing NR52 bit 7 when the APU is already on is not a power-on, so it must
        leave the frame sequencer's step pointer alone. Ocelot used to reset the step to
        0 on any bit-7 write, which pulled the next length clock a whole frame step
        (8192 T-cycles) earlier than hardware. blargg @07-len sweep period sync@ subtest
        5 measures exactly this: it writes NR52=$80 to an already-on APU and requires the
        next length clock to stay two steps away, not one.

        The write sequence below mirrors the ROM's, including the NR14=$40 pre-write.
        That write turns length-enable on while it is still off, so the later NR14=$C0
        has no 0->1 transition on bit 6 and cannot fire the extra-length-clock quirk,
        which would otherwise disable the channel immediately and mask the timing. -}
        let framePeriodM = 2048 -- 8192 T-cycles, one frame-sequencer step

            -- Leave the sequencer with an odd next step, so the next event does not
            -- clock length and the one after it does.
            armChannel1 apu = do
                write8 0xFF26 0x80 apu -- power on: step 0 next, full period to go
                advance framePeriodM apu -- fires step 0, so step 1 is next
                write8 0xFF14 0x40 apu -- length enable on (no trigger)
                write8 0xFF11 0x3F apu -- length = 1
                write8 0xFF12 0x08 apu -- DAC on, silent
                write8 0xFF14 0xC0 apu -- trigger, length still enabled
            ch1On apu = do
                v <- read8 0xFF26 apu
                pure (v .&. 0x01)

        it "keeps the length clock two steps away when bit 7 is written to an already-on APU" $ do
            apu <- initial
            armChannel1 apu
            write8 0xFF26 0x80 apu -- already on: must not reset the step
            advance framePeriodM apu -- fires step 1: no length clock
            afterOne <- ch1On apu
            afterOne `shouldBe` 0x01
            advance framePeriodM apu -- fires step 2: length clock disables the channel
            afterTwo <- ch1On apu
            afterTwo `shouldBe` 0x00

        it "clocks length on the next step after a genuine off-to-on power cycle" $ do
            apu <- initial
            armChannel1 apu
            write8 0xFF26 0x00 apu -- power off
            write8 0xFF26 0x80 apu -- genuine power-on: step resets to 0
            -- Power-off cleared the channel, so re-arm before timing the clock.
            write8 0xFF14 0x40 apu
            write8 0xFF11 0x3F apu
            write8 0xFF12 0x08 apu
            write8 0xFF14 0xC0 apu
            advance framePeriodM apu -- fires step 0: clocks length immediately
            afterOne <- ch1On apu
            afterOne `shouldBe` 0x00

    describe "channel 3 sample timing" $ do
        {- Wave RAM at 0xFF30-0xFF3F is only directly addressable while channel 3 is
        off. Once the channel is running, a read is redirected to the byte holding the
        sample the channel is currently on (CGB), or blocked outright (DMG). Both tests
        below therefore load the wave bytes before triggering, then read back through
        that redirect to observe the channel's sample position without exporting it. -}
        let armWave apu = do
                write8 0xFF26 0x80 apu -- APU on
                write8 0xFF1A 0x80 apu -- ch3 DAC on
                write8 0xFF30 0xAA apu -- wave byte 0 (samples 0 and 1)
                write8 0xFF31 0xBB apu -- wave byte 1 (samples 2 and 3)
        it "delays the first sample fetch after an NR34 trigger" $ do
            {- freq 2046 gives a 4 T-cycle period, so the first fetch is due at 4 + delay
            and every 4 T-cycles after. Reading at 12 and then 16 T-cycles brackets the
            delay: 0xAA at 12 rules out a delay of 4 or less (the position would already
            have reached wave byte 1), and 0xBB at 16 rules out 10 or more (the position
            would still be on byte 0).

            This pins the delay to 5-8 T-cycles rather than to exactly 6. 'advance' is
            M-cycle granular, so a read can only land on a multiple of 4 T-cycles, and
            fetch times are always even; delays of 6 and 8 put the fetch at 10 and 12,
            which no such read can separate. 6 is SameBoy's value. -}
            apu <- initial
            setCgbMode True apu
            armWave apu
            write8 0xFF1D 0xFE apu -- NR33: frequency low
            write8 0xFF1E 0x87 apu -- NR34: trigger, frequency high 7 -> freq 2046
            advance 3 apu -- 12 T-cycles: first fetch due at 10, so position 1, byte 0
            atTwelve <- read8 0xFF30 apu
            atTwelve `shouldBe` 0xAA
            advance 1 apu -- 16 T-cycles: position 2, byte 1
            atSixteen <- read8 0xFF30 apu
            atSixteen `shouldBe` 0xBB

        {- On DMG a wave read is redirected to the current sample byte only inside the
        window where the channel just read it, and returns 0xFF outside. The pair below
        brackets the window's width: it must be open when the read lands on the fetch
        itself, and shut 2 T-cycles later. That pins the width to 1-2 T-cycles; as above,
        M-cycle granular reads cannot separate those two, because every reachable offset
        past an (always even) fetch is even. Hardware's window is one 2 T-cycle APU step,
        and Ocelot previously modelled it as 4, which left it open at offset 2. -}
        it "redirects a DMG wave read landing on the channel's own fetch" $ do
            apu <- initial
            armWave apu
            write8 0xFF1D 0xFF apu -- NR33: frequency low
            write8 0xFF1E 0x87 apu -- NR34: trigger, frequency high 7 -> freq 2047
            advance 2 apu -- 8 T-cycles: 2 T-cycle period, so the fetch lands exactly here
            v <- read8 0xFF30 apu
            v `shouldBe` 0xAA

        it "blocks a DMG wave read landing 2 T-cycles after the channel's own fetch" $ do
            apu <- initial
            armWave apu
            write8 0xFF1D 0xFC apu -- NR33: frequency low
            write8 0xFF1E 0x87 apu -- NR34: trigger, frequency high 7 -> freq 2044
            advance 4 apu -- 16 T-cycles: 8 T-cycle period puts the fetch at 14
            v <- read8 0xFF30 apu
            v `shouldBe` 0xFF

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

        -- The blargg dmg_sound `01-registers` ROM verifies that after writing 0xFF to each
        -- NR register, the read returns the value OR'd with the register's "unused-bits-read-as-1"
        -- mask. We can't run that ROM end-to-end (it reports via the LCD, not serial), so lock the
        -- masks in here to catch regressions from a regression in 'readRegister'.
        mapM_ regMaskCase nrReadMasks

    describe "blargg test_rw walk" $ do
        -- Mirror dmg_sound 01-registers test 2: for every register from NR10..NR51 (skipping NR52)
        -- and every D in 0..255, write D, expect read == (D | mask). Between iterations the ROM
        -- mutes panning and disables the wave channel, so we do the same.
        it "every (addr, D) round-trips read = D | mask" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- Power on
            failures <- collectRwFailures apu
            failures `shouldBe` []

    describe "sweep overflow on trigger" $ do
        it "trigger with shift>0 and shadow+shadow>>shift > 0x7FF disables ch1" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- Power on
            write8 0xFF12 0x08 apu -- DAC on (bits 7..3 nonzero)
            -- NR10: sweep period=0 (timer treated as 8), no negate, shift=1.
            write8 0xFF10 0x01 apu
            -- Frequency = 0x7FF. On trigger: 0x7FF + (0x7FF >> 1) = 0xBFE which exceeds 11 bits,
            -- so ch1 is immediately disabled.
            write8 0xFF13 0xFF apu
            write8 0xFF14 0x87 apu
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x00

    describe "length-counter extra clock quirk" $ do
        -- After one frame-sequencer step (2048 M-cycles), the next step is step 1 which does not
        -- clock the length counter, putting us in the "first half" of a length period.
        it "len-en 0->1 in first half clocks the length counter" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            write8 0xFF12 0xF0 apu -- DAC on, vol 15
            write8 0xFF11 0x3E apu -- Length = 64 - 62 = 2
            write8 0xFF14 0x80 apu -- Trigger, len-en=0
            advance 2048 apu -- Step into first half
            write8 0xFF14 0x40 apu -- Enable length: clocks length 2->1
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x01 -- Still on
            -- Two more frame-sequencer steps reach step 2 (next length tick), which clocks
            -- 1 -> 0 and disables the channel.
            advance 4096 apu
            v' <- read8 0xFF26 apu
            (v' .&. 0x01) `shouldBe` 0x00
        it "trigger with len-en already 1 reloads length=0 and clocks once in first half" $ do
            apu <- initial
            write8 0xFF26 0x80 apu
            write8 0xFF12 0xF0 apu -- DAC on
            write8 0xFF11 0x3F apu -- Length = 64 - 63 = 1
            advance 2048 apu -- First half
            write8 0xFF14 0x40 apu -- Enable length: clocks 1 -> 0, channel disabled
            v <- read8 0xFF26 apu
            (v .&. 0x01) `shouldBe` 0x00
            -- Trigger with len-en still 1: trigger reloads length 0 -> 64, then the post-trigger
            -- extra clock decrements to 63.
            write8 0xFF14 0xC0 apu
            v' <- read8 0xFF26 apu
            (v' .&. 0x01) `shouldBe` 0x01 -- Channel re-enabled
    describe "channel 2 trigger" $ do
        it "writing the trigger bit and a non-zero envelope enables ch2 in NR52" $ do
            apu <- initial
            write8 0xFF26 0x80 apu -- Power APU on (post-boot handoff)
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
            write8 0xFF19 0x80 apu -- Trigger
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
            write8 0xFF19 0x80 apu -- Set NR50 to a known non-zero value.
            write8 0xFF24 0x77 apu -- Power off.
            write8 0xFF26 0x00 apu
            nr50 <- read8 0xFF24 apu
            nr52 <- read8 0xFF26 apu
            nr50 `shouldBe` 0x00
            (nr52 .&. 0x80) `shouldBe` 0x00 -- Power off
            (nr52 .&. 0x0F) `shouldBe` 0x00 -- No channels enabled
        it "while powered off, writes to NR12 (ch1 envelope) are ignored" $ do
            apu <- initial
            write8 0xFF26 0x00 apu -- Power off
            write8 0xFF12 0xF0 apu -- Attempt write while off
            -- After power-on, NR12 should still read as if cleared (not 0xF0).
            write8 0xFF26 0x80 apu -- Power on
            v <- read8 0xFF12 apu
            v `shouldBe` 0x00

        it "DMG vs CGB: NRx1 length writes while powered off differ" $ do
            -- DMG accepts the length value; CGB ignores it. We probe by triggering ch1 with
            -- length-enable after the length write, then driving a few frame-sequencer length steps.
            -- The DMG write loaded length=1, so the channel disables on the very next length step;
            -- the CGB write was discarded so length defaults to 64 and the channel stays on.
            let probe isCgb = do
                    apu <- initial
                    setCgbMode isCgb apu
                    write8 0xFF26 0x00 apu -- Power off
                    write8 0xFF11 0x3F apu -- Length = 1 (DMG only takes effect)
                    write8 0xFF26 0x80 apu -- Power on
                    write8 0xFF12 0xF0 apu -- Envelope volume 15
                    write8 0xFF14 0xC0 apu -- Trigger + length-enable
                    -- Two length-counter steps (each 16384 T-cycles).
                    advance 8192 apu
                    advance 8192 apu
                    nr52 <- read8 0xFF26 apu
                    pure (nr52 .&. 0x01)
            dmgBit <- probe False
            cgbBit <- probe True
            dmgBit `shouldBe` 0x00 -- Ch1 off on DMG (length expired)
            cgbBit `shouldBe` 0x01 -- Ch1 still on on CGB (length write blocked)
        it "powering off then on leaves channels disabled" $ do
            apu <- initial
            write8 0xFF17 0xF0 apu
            write8 0xFF19 0x80 apu -- Trigger ch2
            write8 0xFF26 0x00 apu -- Power off
            write8 0xFF26 0x80 apu -- Power on
            v <- read8 0xFF26 apu
            (v .&. 0x0F) `shouldBe` 0x00

    describe "advance produces samples" $ do
        it "after triggering ch2 and advancing 1 frame, samples are emitted" $ do
            apu <- freshSquareWaveApu
            advance 17556 apu -- One frame
            samples <- drainSamples apu
            length samples `shouldSatisfy` (> 0)
            -- At least one of the samples should be non-zero (the channel is producing output).
            any (/= 0) samples `shouldBe` True

        it "draining after multiple advances preserves sample order" $ do
            apuFrameByFrame <- freshSquareWaveApu
            advance 17556 apuFrameByFrame
            firstFrame <- drainSamples apuFrameByFrame
            advance 17556 apuFrameByFrame
            secondFrame <- drainSamples apuFrameByFrame

            apuCombined <- freshSquareWaveApu
            advance 17556 apuCombined
            advance 17556 apuCombined
            combined <- drainSamples apuCombined

            combined `shouldBe` (firstFrame ++ secondFrame)

        it "vector drains preserve the same samples as list drains" $ do
            apuList <- freshSquareWaveApu
            advance 17556 apuList
            listSamples <- drainSamples apuList

            apuVector <- freshSquareWaveApu
            advance 17556 apuVector
            vectorSamples <- drainSamplesVector apuVector

            V.toList vectorSamples `shouldBe` listSamples

        it "pointer drains copy bounded samples and clear the queue" $ do
            apuList <- freshSquareWaveApu
            advance 17556 apuList
            expected <- take 16 <$> drainSamples apuList

            apuPtr <- freshSquareWaveApu
            advance 17556 apuPtr
            copied <- allocaArray 16 $ \ptr -> do
                n <- drainSamplesInto ptr 16 apuPtr
                samples <- peekArray n ptr
                pure (n, samples)

            copied `shouldBe` (16, expected)
            drainSamples apuPtr `shouldReturn` []

    describe "high-pass filter" $ do
        it "DC offset decays to near-zero after running a silent (all-DAC-off) APU for many frames" $ do
            -- With all DACs off every channel outputs 0. The APU should have been initialized with
            -- non-zero master volume (NR50 = 0x77) so there is a DC contribution while any DAC is
            -- on. After enough T-cycles the HPF capacitor must have discharged; all samples should
            -- be exactly 0 once steady-state is reached. We advance 60 frames (~1 second).
            apu <- initial
            write8 0xFF26 0x80 apu -- Power on
            write8 0xFF24 0x77 apu -- Master vol 7 both sides
            write8 0xFF25 0x11 apu -- Ch1 panned both sides
            write8 0xFF12 0xF0 apu -- Ch1 DAC on, vol 15
            write8 0xFF14 0x80 apu -- Trigger ch1
            advance 17556 apu -- Let it run for a frame with ch1 active
            -- Now cut the DAC: writing 0x00 to NR12 turns the DAC off.
            write8 0xFF12 0x00 apu
            -- Drain any buffered samples so far.
            _ <- drainSamples apu
            -- Run for 60 frames with all DACs off; the HPF capacitor should fully discharge.
            mapM_ (\_ -> advance 17556 apu) [1 .. 60 :: Int]
            samples <- drainSamples apu
            -- Take the last 800 samples (final ~8 ms) and verify all are zero.
            let recent = drop (length samples - 800) samples
            all (== 0) recent `shouldBe` True

        it "a sustained square wave has near-zero DC component after the filter settles" $ do
            -- Run a 50% duty square wave at volume 8 for ~1 second. The waveform is symmetric
            -- (+v, -v alternating) so its true DC is 0. After the filter settles (a few hundred ms)
            -- the running average should be within 1 LSB of zero.
            apu <- initial
            write8 0xFF26 0x80 apu
            write8 0xFF24 0x77 apu
            write8 0xFF25 0x11 apu -- Ch1 left and right
            write8 0xFF12 0x88 apu -- Vol 8, env down, period 0
            write8 0xFF11 0x80 apu -- 50% duty
            write8 0xFF13 0x00 apu
            write8 0xFF14 0x87 apu -- Trigger, freq = 7 -> very low freq, but DAC is on
            -- Advance 60 frames to let the filter settle.
            mapM_ (\_ -> advance 17556 apu) [1 .. 60 :: Int]
            _ <- drainSamples apu
            -- Advance one more frame and collect samples.
            advance 17556 apu
            samples <- drainSamples apu
            let n = length samples
            -- Mean of left channel samples (every other sample starting at index 0).
            let leftSamples = [fromIntegral s :: Double | (i, s) <- zip [0 :: Int ..] samples, even i]
                mean = sum leftSamples / fromIntegral (length leftSamples)
            -- After ~1 second, DC component should be well below 1% of full scale (32767).
            abs mean `shouldSatisfy` (< 328)
            n `shouldSatisfy` (> 0)

    describe "advance batching equivalence" $ do
        -- 'Ocelot.Bus' defers the APU: it accumulates peripheral M-cycles in
        -- 'busApuDebt' and settles them in one 'advance' call at the next APU
        -- register touch or sample drain, instead of stepping 1 M-cycle at a
        -- time. That is only sound if batching is bit-exact, which holds
        -- because 'computeChunk' takes the minimum of every channel timer, the
        -- frame-sequencer timer, and the sample-emission countdown, so a chunk
        -- can never step past an event. These tests pin that invariant down.
        it "produces an identical sample stream whatever the batch size" $ do
            let run stepper = do
                    apu <- initial
                    write8 0xFF26 0x80 apu
                    write8 0xFF24 0x77 apu
                    write8 0xFF25 0xF3 apu
                    mapM_
                        ( \(gap, reg, val) -> do
                            stepper gap apu
                            write8 reg val apu
                        )
                        (apuScript 1500)
                    stepper 5000 apu
                    drainSamplesVector apu
            oneAtATime <- run (\gap apu -> mapM_ (\_ -> advance 1 apu) [1 .. gap])
            whole <- run advance
            capped <- run (chunkedAdvance 64)
            V.length oneAtATime `shouldSatisfy` (> 10000)
            whole `shouldBe` oneAtATime
            capped `shouldBe` oneAtATime

        it "leaves identical register state whatever the batch size" $ do
            let run stepper = do
                    apu <- initial
                    write8 0xFF26 0x80 apu
                    write8 0xFF24 0x77 apu
                    write8 0xFF25 0xF3 apu
                    mapM_
                        ( \(gap, reg, val) -> do
                            stepper gap apu
                            write8 reg val apu
                        )
                        (apuScript 1500)
                    stepper 5000 apu
                    mapM_ (`read8` apu) [0xFF10 .. 0xFF3F]
            oneAtATime <- run (\gap apu -> mapM_ (\_ -> advance 1 apu) [1 .. gap])
            whole <- run advance
            whole `shouldBe` oneAtATime

freshSquareWaveApu :: IO ApuState
freshSquareWaveApu = do
    apu <- initial
    write8 0xFF26 0x80 apu -- Power APU on (post-boot handoff)
    -- Set up a 1 kHz square wave: freq = 2048 - 4194304/(32*1000) = 1917.
    -- Encode: low byte = 1917 & 0xFF, high bits = (1917 >> 8) & 7.
    write8 0xFF24 0x77 apu -- Master vol both sides 7
    write8 0xFF25 0x22 apu -- Pan ch2 to both sides
    write8 0xFF16 0x80 apu -- 50% duty
    write8 0xFF17 0xF0 apu -- Vol 15, env down period 0
    write8 0xFF18 0x7D apu -- Freq low (0x77D = 1917)
    write8 0xFF19 0x87 apu -- Trigger + freq high
    pure apu

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
        write8 0xFF26 0x80 apu -- Power on
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

{- | Advance in fixed-size chunks, modelling the bus's debt horizon: never
more than @cap@ M-cycles settle in a single 'advance' call.
-}
chunkedAdvance :: Int -> Int -> ApuState -> IO ()
chunkedAdvance cap = go
  where
    go remaining apu
        | remaining <= 0 = pure ()
        | otherwise = do
            let k = min cap remaining
            advance k apu
            go (remaining - k) apu

{- | A deterministic pseudo-random script of @(gap in M-cycles, register,
value)@ triples covering the whole @0xFF10-0xFF3F@ window. Hand-rolled LCG
rather than QuickCheck so the two runs under comparison see byte-identical
input without needing a shared generator.
-}
apuScript :: Int -> [(Int, Word16, Word8)]
apuScript n = take n (go 12345)
  where
    go s =
        let s1 = (s * 1103515245 + 12345) `mod` 2147483648
            s2 = (s1 * 1103515245 + 12345) `mod` 2147483648
            s3 = (s2 * 1103515245 + 12345) `mod` 2147483648
            gap = 1 + (s1 `mod` 400)
            reg = fromIntegral (0xFF10 + (s2 `mod` 0x30))
            val = fromIntegral (s3 `mod` 256)
         in (gap, reg, val) : go s3
