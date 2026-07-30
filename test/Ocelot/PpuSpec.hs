{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ocelot.PpuSpec (spec) where

import Control.Monad (forM_)
import Data.Bits (testBit, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Ocelot.Ppu
import Test.Hspec

freshOn :: IO PpuState
freshOn = do
    ps <- initialPpu
    writeIORef (ppuLcdc ps) 0x91
    writeIORef (ppuBgp ps) 0xE4
    writeIORef (ppuMode ps) ModeOamScan
    writeIORef (ppuDot ps) 0
    writeIORef (ppuLy ps) 0
    pure ps

writeVram :: PpuState -> [(Int, Word8)] -> IO ()
writeVram ps = mapM_ (uncurry (MV.write (ppuVram ps)))

writeOam :: PpuState -> [(Int, Word8)] -> IO ()
writeOam ps = mapM_ (uncurry (MV.write (ppuOam ps)))

{- | Advance until the current scanline has been drawn, i.e. mode 3 has ended
and 'renderLine' has run.

Mode 3 is variable-length (fine scroll and window activation extend it), so a
test that enables either cannot assume the old fixed @80 + 172@ boundary.
-}
advanceThroughDraw :: PpuState -> IO ()
advanceThroughDraw ps = go (0 :: Int)
  where
    go n
        | n > 456 = pure ()
        | otherwise = do
            m <- readMode ps
            if m == ModeHBlank || m == ModeVBlank
                then pure ()
                else advance 1 ps >> go (n + 4)

{- | Dot at which the current line first reports mode 0, i.e. the end of mode 3.

'advance' moves four dots at a time, so the result is the first M-cycle boundary
at or past the true end. Compare against 'roundUpM' of the expected dot.
-}
hblankStartDot :: PpuState -> IO Int
hblankStartDot ps = go 0
  where
    go !n = do
        m <- readMode ps
        if m == ModeHBlank
            then pure n
            else if n > 456 then pure (-1) else advance 1 ps >> go (n + 4)

-- | Round a dot up to the M-cycle granularity 'hblankStartDot' can observe.
roundUpM :: Int -> Int
roundUpM d = (d + 3) `div` 4 * 4

{- | A PPU parked mid-VBlank with the LYC interrupt source enabled, which is where
mooneye @ppu/stat_lyc_onoff@ sets up each of its rounds.

Dot 8 is past the dot-4 point at which a VBlank line starts comparing, so the
comparison is live and a write to LYC settles the flag immediately.
-}
vblankMatching :: IO PpuState
vblankMatching = do
    ps <- freshOn
    writeIORef (ppuStat ps) 0x40
    writeIORef (ppuLy ps) 144
    writeIORef (ppuMode ps) ModeVBlank
    writeIORef (ppuDot ps) 8
    pure ps

spec :: Spec
spec = do
    describe "mode timing" $ do
        it "after 80 T-cycles (20 M-cycles), Mode 2 -> Mode 3" $ do
            ps <- freshOn
            _ <- advance 20 ps
            m <- readMode ps
            d <- readDot ps
            m `shouldBe` ModeDrawing
            d `shouldBe` 80

        it "after 80+172 T-cycles, Mode 3 -> Mode 0" $ do
            ps <- freshOn
            _ <- advance ((80 + 172) `div` 4) ps
            m <- readMode ps
            d <- readDot ps
            m `shouldBe` ModeHBlank
            d `shouldBe` 252

        {- The DMG OAM bug: a CPU access anywhere in 0xFE00-0xFEFF while the PPU is scanning OAM
        corrupts the row it is scanning, even though the access itself reads 0xFF. SameBoy's
        'GB_trigger_oam_bug' glitches the row's first word against the two rows above it with
        @((a^c) & (b^c)) ^ c@ and then copies bytes 2..7 down from the previous row.

        'accessedOamRow' names row 8 for the scan's first 4 dots and 8 bytes more every 4 dots after,
        matching SameBoy advancing it once per pair of objects. Row 0 is never scanned, and the last 4
        dots of the scan compute a row past the end of OAM, so the corruption window is 76 of the
        scan's 80 dots. blargg 'oam_bug/4-scanline_timing' measures both of those edges.
        -}
        describe "DMG OAM bug" $ do
            let oamAt ps = mapM (MV.read (ppuOam ps))
                inMode2At d = do
                    ps <- freshOn
                    writeIORef (ppuMode ps) ModeOamScan
                    writeIORef (ppuDot ps) d
                    pure ps

            it "walks the scanned row 8 bytes every 4 dots, starting at row 8" $ do
                ps <- inMode2At 0
                rows <- mapM (\d -> writeIORef (ppuDot ps) d >> accessedOamRow ps) [0, 3, 4, 7, 8, 12, 72]
                rows `shouldBe` [8, 8, 16, 16, 24, 32, 152]

            it "reports no row for the last 4 dots of the scan, which address past OAM" $ do
                ps <- inMode2At 0
                rows <- mapM (\d -> writeIORef (ppuDot ps) d >> accessedOamRow ps) [76, 79]
                rows `shouldBe` [-1, -1]

            it "reports no row outside OAM scan" $ do
                ps <- freshOn
                writeIORef (ppuMode ps) ModeDrawing
                writeIORef (ppuDot ps) 100
                r <- accessedOamRow ps
                r `shouldBe` (-1)

            it "glitches the scanned row's first word and copies bytes 2..7 down" $ do
                ps <- inMode2At 0 -- row 8
                -- Row 0 spans bytes 0..7, so the word at row-4 (bytes 4,5) is also part of the copy
                -- source. Keep those two zero so the glitch input stays easy to read off:
                -- word 0 = 0x00FF, word 4 = 0x0000, word 8 = 0xFF00, all little-endian.
                mapM_ (uncurry (MV.write (ppuOam ps))) $
                    [ (0, 0xFF)
                    , (1, 0x00)
                    , (2, 0x21)
                    , (3, 0x22)
                    , (4, 0x00)
                    , (5, 0x00)
                    , (6, 0x25)
                    , (7, 0x26)
                    , (8, 0x00)
                    , (9, 0xFF)
                    ]
                        <> [(i, 0x00) | i <- [10 .. 15]]
                triggerOamBug 0xFE00 ps
                -- ((0xFF00 ^ 0) & (0x00FF ^ 0)) ^ 0 = 0
                glitched <- oamAt ps [8, 9]
                copied <- oamAt ps [10 .. 15]
                source <- oamAt ps [2 .. 7]
                glitched `shouldBe` [0x00, 0x00]
                copied `shouldBe` source

            it "leaves OAM alone for an address outside 0xFE00-0xFEFF" $ do
                ps <- inMode2At 0
                mapM_ (uncurry (MV.write (ppuOam ps))) [(8, 0x11), (9, 0x22)]
                triggerOamBug 0xC000 ps
                v <- oamAt ps [8, 9]
                v `shouldBe` [0x11, 0x22]

            it "leaves OAM alone on CGB" $ do
                ps <- inMode2At 0
                setCgbMode True ps
                mapM_ (uncurry (MV.write (ppuOam ps))) [(8, 0x11), (9, 0x22)]
                triggerOamBug 0xFE00 ps
                v <- oamAt ps [8, 9]
                v `shouldBe` [0x11, 0x22]

            {- A CPU *read* corrupts differently from a write, and it also samples the scan a
            row later, because 'cycleRead' ticks the bus before performing the access. Both
            were wrong together, and the row error hid the pattern error: it sent blargg
            @oam_bug/8-instr_effect@'s @POP rp@ subtest down the row-0x40 quaternary branch
            instead of the secondary one, so correcting the patterns alone moved nothing.
            -}
            it "reads corrupt a row earlier than writes at the same dot" $ do
                ps <- inMode2At 4
                write <- accessedOamRow ps
                -- Row 8 is what a bus read sees where the address bus sees 16.
                mapM_ (uncurry (MV.write (ppuOam ps))) [(8, 0x11), (9, 0x22), (16, 0x33), (17, 0x44)]
                triggerOamBugRead 0xFE00 ps
                untouched <- oamAt ps [16, 17]
                (write, untouched) `shouldBe` (16, [0x33, 0x44])

            it "a read copies all eight bytes of the row above, where a write copies six" $ do
                ps <- inMode2At 4 -- a read here works on row 8
                -- Row 8 has @8 .&. 0x18 == 8@, so it takes the plain read glitch
                -- @b .|. (a .&. c)@ over words at rows 8, 0, and 4: @0x0000 .|. 0xFFFF@.
                mapM_ (uncurry (MV.write (ppuOam ps))) $
                    [(0, 0x00), (1, 0x00), (2, 0x21), (3, 0x22), (4, 0xFF), (5, 0xFF)]
                        <> [(6, 0x25), (7, 0x26), (8, 0xFF), (9, 0xFF)]
                        <> [(i, 0x00) | i <- [10 .. 15]]
                triggerOamBugRead 0xFE00 ps
                glitched <- oamAt ps [0, 1]
                copied <- oamAt ps [8 .. 15]
                -- All eight bytes come down, so bytes 8..9 carry the glitched word too.
                glitched `shouldBe` [0xFF, 0xFF]
                copied `shouldBe` [0xFF, 0xFF, 0x21, 0x22, 0xFF, 0xFF, 0x25, 0x26]

            it "leaves OAM alone on CGB for a read too" $ do
                ps <- inMode2At 4
                setCgbMode True ps
                mapM_ (uncurry (MV.write (ppuOam ps))) [(8, 0x11), (9, 0x22)]
                triggerOamBugRead 0xFE00 ps
                v <- oamAt ps [8, 9]
                v `shouldBe` [0x11, 0x22]

            it "leaves OAM alone in the last 4 dots of the scan, which name no row" $ do
                ps <- inMode2At 76
                mapM_ (uncurry (MV.write (ppuOam ps))) [(152, 0x11), (153, 0x22)]
                triggerOamBug 0xFE00 ps
                v <- oamAt ps [152, 153]
                v `shouldBe` [0x11, 0x22]

        {- The LY=LYC comparison does not use LY directly. SameBoy keeps a separate
        @ly_for_comparison@ that is -1 (no match possible) at the head of a line and only becomes the
        line number a dot or so later, and 'GB_STAT_update' drives both the STAT bit-2 flag and the
        LYC interrupt source from it. Counting the sleeps in display.c dot by dot, a visible line holds the
        PREVIOUS line number for dots 0-2, is a no-match for dot 3, and only reads the current line from
        dot 4. A VBlank line stores -1 before its first sleep so it is suppressed from dot 0 to 3, and
        line 0 stores 0 rather than -1 so it has no no-match dot.
        -}
        {- Line 153 does not report itself for long. SameBoy writes @LY = 153@ two dots in and then
        @LY = 0@ six dots after that, so for roughly 448 of the line's 456 dots a CPU read of 0xFF44
        returns 0 while the PPU is still on line 153 (display.c, the line-153 block).

        Holding 153 for the whole line is what made every blargg APU subtest and mooneye
        @oam_dma_start@ diverge: they sync on LY at the frame wrap and read 153 where hardware reads 0.
        This is a register view only; 'ppuLy' stays the internal line counter.
        -}
        describe "LY on line 153" $ do
            let lyAtDot ps d = writeIORef (ppuDot ps) d >> read8 0xFF44 ps
                onLine153 = do
                    ps <- freshOn
                    writeIORef (ppuLy ps) 153
                    writeIORef (ppuMode ps) ModeVBlank
                    pure ps

            it "reads 153 only for the first few dots, then 0" $ do
                ps <- onLine153
                early <- lyAtDot ps 4
                atClear <- lyAtDot ps 8
                late <- lyAtDot ps 400
                (early, atClear, late) `shouldBe` (153, 0, 0)

            it "leaves the internal line counter on 153" $ do
                ps <- onLine153
                _ <- lyAtDot ps 400
                ly <- readLy ps
                ly `shouldBe` 153

            it "does not touch any other line" $ do
                ps <- freshOn
                writeIORef (ppuLy ps) 152
                writeIORef (ppuMode ps) ModeVBlank
                v <- lyAtDot ps 400
                v `shouldBe` 152

        describe "LYC comparison delay" $ do
            let atDot ps d = do
                    writeIORef (ppuDot ps) d
                    -- Bit 2 is a latch, so moving the dot does not on its own refresh
                    -- it. Rewriting LYC with the value it already holds runs the
                    -- comparison clock at this dot without changing what is compared,
                    -- which is the shape these tests are here to pin down.
                    lyc <- readIORef (ppuLyc ps)
                    write8 0xFF45 lyc ps
                    (.&. 0x04) <$> read8 0xFF41 ps
                withLine ly mode = do
                    ps <- freshOn
                    writeIORef (ppuLy ps) ly
                    writeIORef (ppuLyc ps) ly
                    writeIORef (ppuMode ps) mode
                    pure ps

            it "matches its own line only from dot 4 on a visible line" $ do
                ps <- withLine 5 ModeOamScan
                before <- mapM (atDot ps) [0, 2, 3]
                at4 <- atDot ps 4
                (before, at4) `shouldBe` ([0, 0, 0], 4)

            it "still matches the PREVIOUS line for the first three dots" $ do
                -- The distinctive part: dots 0-2 are not suppressed, they hold line-1.
                ps <- freshOn
                writeIORef (ppuLy ps) 5
                writeIORef (ppuLyc ps) 4
                writeIORef (ppuMode ps) ModeOamScan
                held <- mapM (atDot ps) [0, 1, 2]
                gone <- mapM (atDot ps) [3, 4]
                (held, gone) `shouldBe` ([4, 4, 4], [0, 0])

            it "wraps to 153 for the first three dots of line 0" $ do
                ps <- freshOn
                writeIORef (ppuLy ps) 0
                writeIORef (ppuLyc ps) 153
                writeIORef (ppuMode ps) ModeOamScan
                held <- atDot ps 0
                gone <- atDot ps 3
                (held, gone) `shouldBe` (4, 0)

            it "has no no-match dot on line 0, which stores 0 rather than -1" $ do
                ps <- withLine 0 ModeOamScan
                at3 <- atDot ps 3
                at3 `shouldBe` 4

            it "is suppressed from dot 0 to 3 on a VBlank line" $ do
                ps <- withLine 144 ModeVBlank
                before <- mapM (atDot ps) [0, 3]
                at4 <- atDot ps 4
                (before, at4) `shouldBe` ([0, 0], 4)

            {- The suppression window only means anything if the dot walk stops at the compare dot.
            'statEdge' runs from 'transition', so without a stop at dot 1 the LYC rising edge would be
            deferred to the mode 2 -> 3 boundary at dot 80. That is not academic: wiring the window in
            without the stop shifted every LYC interrupt ~80 dots and changed the dmg-acid2 hash.
            -}
            it "raises the STAT IRQ at the compare dot, not at the next mode boundary" $ do
                ps <- freshOn
                writeIORef (ppuLy ps) 5
                writeIORef (ppuLyc ps) 5
                writeIORef (ppuStat ps) 0x40 -- LYC source enabled
                writeIORef (ppuMode ps) ModeOamScan
                writeIORef (ppuDot ps) 0
                writeIORef (ppuPrevStatLine ps) False
                irqs <- advance 1 ps -- 4 dots, crossing the dot-1 compare event
                (irqs .&. 0x02) `shouldBe` 0x02

            it "still reports no match when LY and LYC differ" $ do
                ps <- freshOn
                writeIORef (ppuLy ps) 5
                writeIORef (ppuLyc ps) 9
                v <- atDot ps 40
                v `shouldBe` 0

        {- The STAT mode bits lag the PPU's actual mode by 4 dots. SameBoy carries this as a standing
        note ("the STAT register's mode bits are always late by 4 T-cycles"), and the trace confirms
        it: at the dot-80 mode 2 -> 3 boundary Ocelot reported mode 3 where SameBoy still read mode 2.

        Only the register view is delayed. The STAT interrupt line keeps using the real mode, matching
        SameBoy's separate @mode_for_interrupt@, which is what keeps the interrupt-timing ROMs
        (@intr_1_2_timing-GS@, @intr_2_0_timing@, @stat_irq_blocking@, blargg @interrupt_time@) intact.
        -}
        describe "STAT mode-bit delay" $ do
            it "still reports mode 2 at the dot-80 start of drawing" $ do
                ps <- freshOn
                _ <- advance 20 ps -- dot 80: mode 3 has begun internally
                atBoundary <- read8 0xFF41 ps
                _ <- advance 1 ps -- dot 84, past the delay
                afterDelay <- read8 0xFF41 ps
                (atBoundary .&. 0x03, afterDelay .&. 0x03) `shouldBe` (2, 3)

            it "switches to mode 3 at exactly dot 84, pinning the delay at 4" $ do
                -- Stepping alone cannot tell 3 from 4: dot 80 reads mode 2 and dot 84 mode 3 under
                -- either. Drive the dot to bracket the boundary to a single dot. A delay of 3 was
                -- tried and is worse: the traced divergence moves much earlier.
                ps <- freshOn
                _ <- advance 20 ps -- internal mode 3, dot 80
                writeIORef (ppuDot ps) 83
                at83 <- read8 0xFF41 ps
                writeIORef (ppuDot ps) 84
                at84 <- read8 0xFF41 ps
                (at83 .&. 0x03, at84 .&. 0x03) `shouldBe` (2, 3)

            it "leaves the internal mode undelayed" $ do
                ps <- freshOn
                _ <- advance 20 ps
                m <- readMode ps
                m `shouldBe` ModeDrawing

            it "reports mode 0 immediately when the LCD is off" $ do
                ps <- freshOn
                write8 0xFF40 0x00 ps
                v <- read8 0xFF41 ps
                (v .&. 0x03) `shouldBe` 0

            it "reports mode 0 at power-on, when the LCD is off but the mode is not HBlank" $ do
                -- 'initialPpu' powers on with the LCD off and 'ppuMode' at ModeOamScan, which every
                -- boot-ROM machine passes through. Reporting the internal mode here gave mode 2.
                ps <- initialPpu
                v <- read8 0xFF41 ps
                (v .&. 0x03) `shouldBe` 0

            it "holds mode 0 for 5 dots on entry to VBlank, not 4" $ do
                -- SameBoy's line-144 path sleeps 2 + 2 + 1 before setting mode 1.
                ps <- freshOn
                _ <- advance (144 * 114) ps -- exactly the start of line 144
                writeIORef (ppuDot ps) 4 -- dot 4: still inside the 5-dot window
                at4 <- read8 0xFF41 ps
                writeIORef (ppuDot ps) 5 -- dot 5: mode 1 becomes visible
                at5 <- read8 0xFF41 ps
                (at4 .&. 0x03, at5 .&. 0x03) `shouldBe` (0, 1)

        -- The first scanline after LCDC bit 7 goes 0 -> 1 is special on hardware: it has no mode 2 at
        -- all, drawing starts at dot 78, and the line runs 448 dots, each figure gaining one dot on
        -- DMG. Without this the PPU line phase sits 8 T-cycles late against SameBoy forever after
        -- (every LY edge and the VBlank IRQ with it).
        describe "first scanline after the LCD is enabled" $ do
            -- 'initialPpu' is DMG; pass True for a CGB machine. On CGB both the mode 3 start and the
            -- line length drop by one, because SameBoy spends an extra dot before the post-enable line
            -- begins on DMG only.
            let turnOnHost cgb = do
                    ps <- initialPpu
                    setCgbMode cgb ps
                    write8 0xFF40 0x11 ps -- LCD off
                    write8 0xFF40 0x91 ps -- LCD on: starts the short line
                    pure ps
                turnOn = turnOnHost False

            {- Pinning the mode 3 start to a single dot needs a *pure* observation: 'advance' moves
            4 dots at a time, so stepping alone cannot tell 79 from 80, and an earlier attempt here
            used @advance 0@, which is a no-op ('stepDots' returns immediately on 0) and asserted
            nothing at all.

            Reading STAT is pure, and on this line 'visibleModeBits' reports mode 3 from @start@
            itself ('statModeDelayFor' is zero here), so walking the dot counter across that flip
            locates @start@ exactly. On the enable line the mode before drawing is 0, not 2, so the
            flip is 0 -> 3.
            -}
            let statModeAtDot ps d = do
                    writeIORef (ppuDot ps) d
                    (.&. 0x03) <$> read8 0xFF41 ps
                drawStartOf cgb = do
                    ps <- turnOnHost cgb
                    _ <- advance 25 ps -- comfortably into mode 3 (100 dots)
                    m <- readMode ps
                    m `shouldBe` ModeDrawing
                    let probe d
                            | d > 120 = pure (-1)
                            | otherwise = do
                                v <- statModeAtDot ps d
                                if v == 3 then pure d else probe (d + 1)
                    probe 0

            it "starts drawing at exactly dot 79 on DMG" $ do
                start <- drawStartOf False
                start `shouldBe` 79

            it "starts drawing at exactly dot 78 on CGB" $ do
                start <- drawStartOf True
                start `shouldBe` 78

            it "runs exactly 449 dots on DMG and 448 on CGB" $ do
                {- Reads the line length to a single dot without teleporting the counter. Stepping one
                M-cycle at a time, the step that ends the line consumes only as many dots as the line
                had left and carries the rest into line 1, where 'hblankLineEnd' has reset the counter
                to 0. So the dot observed just after LY ticks is @4k - lineLength@ for the first
                multiple of 4 at or past the length, which separates 448 (leftover 0), 449 (3), and
                450 (2). Comparing last-dot-before-the-wrap instead only resolves 4 dots.
                -}
                let leftoverOn cgb = do
                        ps <- turnOnHost cgb
                        let go !n
                                | n > 130 = pure (-1)
                                | otherwise = do
                                    ly <- readLy ps
                                    if ly == 1 then readDot ps else advance 1 ps >> go (n + 1)
                        go (0 :: Int)
                dmg <- leftoverOn False
                cgb <- leftoverOn True
                (dmg, cgb) `shouldBe` (3, 0)

            {- Hardware runs no mode 2 at all on this line: SameBoy clears the
            STAT mode bits to 0 and leaves OAM and VRAM unblocked for the whole
            pre-drawing window (display.c, "Handle mode 2 on the very first line 0"),
            then goes straight to mode 3. Reporting mode 2 here fabricates an
            OAM-source STAT interrupt hardware never raises and blocks OAM reads
            hardware allows.
            -}
            it "reports STAT mode 0, not mode 2, before drawing starts" $ do
                ps <- turnOn
                stat0 <- read8 0xFF41 ps
                (stat0 .&. 0x03) `shouldBe` 0
                _ <- advance 18 ps -- 72 T-cycles, still short of the drawing start
                stat1 <- read8 0xFF41 ps
                (stat1 .&. 0x03) `shouldBe` 0

            it "reports STAT mode 3 once drawing starts" $ do
                ps <- turnOn
                -- Drawing starts at dot 79 on DMG, and the mode bits lag it by 4, so 84 dots in.
                _ <- advance 21 ps
                stat <- read8 0xFF41 ps
                (stat .&. 0x03) `shouldBe` 3

            it "runs short, so LY increments earlier than a full line" $ do
                ps <- turnOn
                _ <- advance 112 ps -- 448 T-cycles: DMG's line 0 runs 449, so still on it
                before <- readLy ps
                _ <- advance 1 ps -- 452 T-cycles: past 449, line 0 has ended
                after <- readLy ps
                (before, after) `shouldBe` (0, 1)

            it "returns to full 456-dot lines after the first one" $ do
                ps <- turnOn
                _ <- advance 113 ps -- through the short line 0 (449 dots)
                lineOneStart <- readLy ps
                -- Line 1 then runs the full 456 from dot 0. 449 + 456 = 905, so 904 T-cycles in
                -- total is still line 1 and 908 has ended it.
                _ <- advance 113 ps -- 904 T-cycles
                before <- readLy ps
                _ <- advance 1 ps -- 908 T-cycles
                after <- readLy ps
                (lineOneStart, before, after) `shouldBe` (1, 1, 2)

        -- Mode 3 is not a fixed 172 dots on hardware: the fetcher discards
        -- SCX mod 8 pixels at the left edge, and activating the window costs a
        -- fetcher restart. Whatever mode 3 takes, mode 0 gives back, so the
        -- scanline stays 456 dots. Drives the mooneye ppu/*_timing ROMs.
        it "SCX mod 8 extends mode 3 and shortens HBlank by the same amount" $ do
            let endsAt scx = do
                    ps <- freshOn
                    writeIORef (ppuScx ps) scx
                    -- Step one dot at a time so we catch the exact boundary.
                    let go !n = do
                            m <- readMode ps
                            if m == ModeHBlank
                                then pure n
                                else
                                    if n > 456
                                        then pure (-1)
                                        else advance 1 ps >> go (n + 4)
                    go 0
            forM_ [0 .. 7 :: Int] $ \k -> do
                got <- endsAt (fromIntegral k)
                -- advance() steps 4 dots at a time, so round up to the M-cycle
                -- boundary that first lands at or past the true end of mode 3.
                let expected = ((172 + k + 80) + 3) `div` 4 * 4
                (k, got) `shouldBe` (k, expected)

        it "keeps the scanline at 456 dots whatever SCX is" $ do
            forM_ [0, 3, 7 :: Word8] $ \scx -> do
                ps <- freshOn
                writeIORef (ppuScx ps) scx
                _ <- advance 114 ps
                ly <- readLy ps
                m <- readMode ps
                (scx, ly, m) `shouldBe` (scx, 1, ModeOamScan)

        it "activating the window costs 6 extra dots of mode 3" $ do
            ps <- freshOn
            -- LCDC bit 5 enables the window; WX=7/WY=0 puts it over the line.
            writeIORef (ppuLcdc ps) 0xB1
            writeIORef (ppuWy ps) 0
            writeIORef (ppuWx ps) 7
            let go !n = do
                    m <- readMode ps
                    if m == ModeHBlank
                        then pure n
                        else if n > 456 then pure (-1) else advance 1 ps >> go (n + 4)
            got <- go 0
            got `shouldBe` ((172 + 6 + 80) + 3) `div` 4 * 4

        {- Objects stall the pixel fetcher, so a line carrying them spends longer in
        mode 3 than the sprite-free 172. Pandocs gives the per-object cost as
        @11 - min(5, (x + SCX) mod 8)@, and that is what takes mode 3 to its
        documented 289-dot maximum: ten objects at 11 dots each on top of the 172
        base and a fine scroll of 7. Drives mooneye @ppu/intr_2_mode0_timing_sprites@.
        -}
        it "an object at a tile boundary extends mode 3 by 11 dots" $ do
            ps <- freshOn
            writeIORef (ppuLcdc ps) 0x93 -- 'freshOn' plus OBJ enable (bit 1)
            -- Y=16 puts the object's top row on LY=0; X=8 puts it at screen x=0.
            writeOam ps [(0, 16), (1, 8), (2, 0), (3, 0)]
            got <- hblankStartDot ps
            got `shouldBe` roundUpM (80 + 172 + 11)

        it "an object's penalty shrinks as its fine offset into the tile grows" $ do
            -- The penalty falls by one dot per unit of @(x + SCX) mod 8@ until it
            -- bottoms out at 6, while the fine scroll adds that same amount back.
            -- The two therefore cancel up to SCX=5 and only then start to diverge.
            forM_ (zip [0 .. 7 :: Word8] [11, 11, 11, 11, 11, 11, 12, 13]) $
                \(scx, extra) -> do
                    ps <- freshOn
                    writeIORef (ppuLcdc ps) 0x93
                    writeIORef (ppuScx ps) scx
                    writeOam ps [(0, 16), (1, 8), (2, 0), (3, 0)]
                    got <- hblankStartDot ps
                    (scx, got) `shouldBe` (scx, roundUpM (80 + 172 + extra))

        it "ten objects on a tile boundary reach the 289-dot mode 3 maximum" $ do
            ps <- freshOn
            writeIORef (ppuLcdc ps) 0x93
            writeIORef (ppuScx ps) 7
            -- Ten objects, each on a distinct tile boundary so each pays the full 11.
            -- The eleventh is dropped by the ten-per-line OAM scan limit and so is
            -- free, which is what pins the cap rather than the object count.
            writeOam ps $
                concat
                    [ [(i * 4, 16), (i * 4 + 1, fromIntegral (8 + 8 * i + 1)), (i * 4 + 2, 0), (i * 4 + 3, 0)]
                    | i <- [0 .. 10]
                    ]
            got <- hblankStartDot ps
            got `shouldBe` roundUpM (80 + 289)

        {- The abort is charged per background tile, not per object. Charging it per
        object would make this case 110 dots instead of 65, and the ROM's
        @testcase 16, 0,0,...@ line rejects that.
        -}
        it "objects sharing a background tile pay the fetch abort only once" $ do
            ps <- freshOn
            writeIORef (ppuLcdc ps) 0x93
            -- Ten objects stacked at OAM X=0: 6 dots each plus a single 5-dot abort.
            writeOam ps $
                concat
                    [ [(i * 4, 16), (i * 4 + 1, 0), (i * 4 + 2, 0), (i * 4 + 3, 0)]
                    | i <- [0 .. 9]
                    ]
            got <- hblankStartDot ps
            got `shouldBe` roundUpM (80 + 172 + 65)

        it "objects off the right edge cost nothing but still fill scan slots" $ do
            ps <- freshOn
            writeIORef (ppuLcdc ps) 0x93
            -- OAM X=168 puts an object at screen x=160, entirely past the last pixel.
            -- Ten of them are free, and they leave no slot for the eleventh, which sits
            -- on screen and would otherwise have cost 11 dots.
            writeOam ps $
                concat
                    [ [(i * 4, 16), (i * 4 + 1, 168), (i * 4 + 2, 0), (i * 4 + 3, 0)]
                    | i <- [0 .. 9]
                    ]
                    ++ [(40, 16), (41, 8), (42, 0), (43, 0)]
            got <- hblankStartDot ps
            got `shouldBe` roundUpM (80 + 172)

        it "objects cost nothing while LCDC bit 1 leaves them disabled" $ do
            ps <- freshOn
            writeOam ps [(0, 16), (1, 8), (2, 0), (3, 0)]
            got <- hblankStartDot ps
            got `shouldBe` roundUpM (80 + 172)

        {- Entering VBlank asserts the mode-2 STAT source as well as the VBlank flag, and on
        CGB the STAT source comes first. SameBoy raises it at dot 2 of line 144 and the
        VBlank flag at dot 5, a gap that straddles an M-cycle boundary and so reads as
        exactly one cycle to the CPU. mooneye has one test per behaviour, differing by a
        single @nop@: @acceptance/ppu/vblank_stat_intr-GS@ expects them together,
        @misc/ppu/vblank_stat_intr-C@ expects STAT one cycle earlier.
        -}
        describe "mode-2 STAT source on entry to VBlank" $ do
            let endOfLine143 cgb = do
                    ps <- initialPpu
                    setCgbMode cgb ps
                    writeIORef (ppuLcdc ps) 0x91
                    writeIORef (ppuStat ps) 0x20 -- OAM/mode-2 source enabled
                    writeIORef (ppuMode ps) ModeHBlank
                    writeIORef (ppuLy ps) 143
                    writeIORef (ppuDot ps) 448
                    writeIORef (ppuPrevStatLine ps) False
                    pure ps

            it "fires one M-cycle before the VBlank flag on CGB" $ do
                ps <- endOfLine143 True
                first <- advance 1 ps -- dots 448..452, crossing the early STAT dot
                second <- advance 1 ps -- line 144 begins and raises VBlank
                (first .&. 0x02, second .&. 0x01, second .&. 0x02)
                    `shouldBe` (0x02, 0x01, 0x00)

            it "fires together with the VBlank flag on DMG" $ do
                ps <- endOfLine143 False
                first <- advance 1 ps
                second <- advance 1 ps
                (first, second .&. 0x01, second .&. 0x02) `shouldBe` (0, 0x01, 0x02)

        it "after a full scanline, LY := 1, Mode 2" $ do
            ps <- freshOn
            _ <- advance 114 ps
            ly <- readLy ps
            m <- readMode ps
            d <- readDot ps
            ly `shouldBe` 1
            m `shouldBe` ModeOamScan
            d `shouldBe` 0

        it "after 144 scanlines, VBlank fires; LY := 144, Mode 1" $ do
            ps <- freshOn
            irqs <- advance (144 * 114) ps
            ly <- readLy ps
            m <- readMode ps
            ly `shouldBe` 144
            m `shouldBe` ModeVBlank
            (irqs .&. 0x01) `shouldBe` 0x01

        it "after a full frame, back to LY=0 Mode 2" $ do
            ps <- freshOn
            _ <- advance (154 * 114) ps
            ly <- readLy ps
            m <- readMode ps
            ly `shouldBe` 0
            m `shouldBe` ModeOamScan

        it "VBlank fires exactly once in a full frame" $ do
            ps <- freshOn
            irqs <- advance (154 * 114) ps
            (irqs .&. 0x01) `shouldBe` 0x01

        it "LCD off freezes the PPU" $ do
            ps <- freshOn
            writeIORef (ppuLcdc ps) 0x11
            irqs <- advance (154 * 114) ps
            ly <- readLy ps
            ly `shouldBe` 0
            irqs `shouldBe` 0

    describe "BG rendering" $ do
        it "renders a striped tile through the BGP identity palette" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00), (2, 0x00), (3, 0xFF)]
            _ <- advance ((80 + 172) `div` 4) ps
            fb <- framebuffer ps
            fb V.! 0 `shouldBe` 0x01

        it "BGP transforms color indices to shades" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            writeIORef (ppuBgp ps) 0xFC
            _ <- advance ((80 + 172) `div` 4) ps
            fb <- framebuffer ps
            fb V.! 0 `shouldBe` 0x03

        it "RGB byte snapshots match immutable RGB framebuffer snapshots" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            rgbBytes <- framebufferRgbBytes ps
            BS.unpack rgbBytes `shouldBe` V.toList rgb

        it "RGB framebuffer copies reuse caller-provided storage" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            fp <- BSI.mallocByteString (framebufferWidth * framebufferHeight * 3)
            let copied = BSI.fromForeignPtr fp 0 (framebufferWidth * framebufferHeight * 3)
            withForeignPtr fp $ \ptr -> copyFramebufferRgb ptr ps
            BS.unpack copied `shouldBe` V.toList rgb

        it "RGB framebuffer copies honor row pitch" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            let rowBytes = framebufferWidth * 3
                pitch = rowBytes + 1
                totalBytes = framebufferHeight * pitch
                expected = pitchedRgb rowBytes pitch (V.toList rgb)
            fp <- BSI.mallocByteString totalBytes
            withForeignPtr fp $ \ptr -> do
                mapM_ (\i -> pokeByteOff ptr i (0xAA :: Word8)) [0 .. totalBytes - 1]
                copyFramebufferRgbWithPitch ptr pitch ps
                copied <- mapM (\i -> peekByteOff ptr i :: IO Word8) [0 .. totalBytes - 1]
                copied `shouldBe` expected

        it "RGBA byte snapshots expand RGB pixels with opaque alpha" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            rgbaBytes <- framebufferRgbaBytes ps
            BS.unpack rgbaBytes `shouldBe` rgbaFromRgb (V.toList rgb)

        it "RGBA framebuffer copies reuse caller-provided storage" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            fp <- BSI.mallocByteString (framebufferWidth * framebufferHeight * 4)
            let copied = BSI.fromForeignPtr fp 0 (framebufferWidth * framebufferHeight * 4)
            withForeignPtr fp $ \ptr -> copyFramebufferRgba ptr ps
            BS.unpack copied `shouldBe` rgbaFromRgb (V.toList rgb)

    describe "register I/O" $ do
        it "STAT read returns mode bits 0..1 from the current mode" $ do
            ps <- freshOn
            -- Step past the 4-dot STAT mode-bit delay; at dot 0 the register still shows the mode
            -- the PPU was in before this one. See "STAT mode-bit delay".
            _ <- advance 1 ps
            v <- read8 0xFF41 ps
            v `shouldBe` 0x86

        {- The coincidence bit is a latch the comparison clock writes, not something
        recomputed when the CPU reads STAT. With the LCD off that clock is stopped, so
        the latch holds and a write to LYC cannot move it. All four rounds of mooneye
        @ppu/stat_lyc_onoff@ turn on this distinction.
        -}
        it "retains the LY=LYC flag while the LCD is off, ignoring LYC writes" $ do
            ps <- vblankMatching
            write8 0xFF45 144 ps -- LYC = LY, so the flag sets while the clock runs
            onMatched <- read8 0xFF41 ps
            write8 0xFF40 0x11 ps -- LCD off
            offRetained <- read8 0xFF41 ps
            write8 0xFF45 1 ps -- clock stopped, so this must not clear the flag
            offAfterLyc <- read8 0xFF41 ps
            map (`testBit` 2) [onMatched, offRetained, offAfterLyc]
                `shouldBe` [True, True, True]

        it "re-runs the comparison against LY=0 when the LCD comes back on" $ do
            -- Round 4: the flag is clear across the LCD-off period and the enable itself
            -- sets it, which is a rising edge on the STAT line and so an interrupt.
            ps <- vblankMatching
            write8 0xFF45 0 ps -- LYC=0 against LY=144: no match
            write8 0xFF40 0x11 ps
            _ <- takePendingStatIrq ps
            write8 0xFF40 0x91 ps -- enable: the comparison is now LY=0 vs LYC=0
            irq <- takePendingStatIrq ps
            stat <- read8 0xFF41 ps
            (irq, testBit stat 2) `shouldBe` (True, True)

        it "raises no IRQ when the LCD comes back on with the flag already set" $ do
            -- Round 2: LY=144 vs LYC=144 sets the flag, the LCD goes off holding it, and
            -- the enable moves the comparison to LY=0 vs LYC=0, which also matches. The
            -- interrupt line never falls, so there is no edge to report.
            ps <- vblankMatching
            write8 0xFF45 144 ps
            write8 0xFF40 0x11 ps
            write8 0xFF45 0 ps
            _ <- takePendingStatIrq ps
            write8 0xFF40 0x91 ps
            irq <- takePendingStatIrq ps
            stat <- read8 0xFF41 ps
            (irq, testBit stat 2) `shouldBe` (False, True)

        it "LY is read-only" $ do
            ps <- freshOn
            writeIORef (ppuLy ps) 0x10
            write8 0xFF44 0x55 ps
            ly <- readLy ps
            ly `shouldBe` 0x10

        it "writing 0 to LCDC bit 7 freezes LY at 0" $ do
            ps <- freshOn
            writeIORef (ppuLy ps) 50
            write8 0xFF40 0x11 ps
            ly <- readLy ps
            m <- readMode ps
            ly `shouldBe` 0
            m `shouldBe` ModeHBlank

    describe "window rendering" $ do
        it "with WX=7,WY=0 the whole line comes from the window tile map" $ do
            ps <- freshOn
            writeVram ps [(16, 0xFF), (17, 0x00), (0x1C00, 0x01)]
            writeIORef (ppuLcdc ps) 0xF1
            writeIORef (ppuWy ps) 0
            writeIORef (ppuWx ps) 7
            writeIORef (ppuBgp ps) 0xE4
            -- The window is on, so mode 3 runs 6 dots past the sprite-free
            -- boundary; step until the line is actually drawn.
            advanceThroughDraw ps
            fb <- framebuffer ps
            fb V.! 0 `shouldBe` 0x01

    describe "FbTarget rendering modes" $ do
        it "FbRgb mode writes the RGB buffer but leaves the RGBA buffer untouched" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            setFbTarget FbRgb ps
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            -- ppuFbRgba is a storable vector; freeze into an immutable VS.Vector.
            -- Alpha bytes are pre-initialised to 255 by initialPpu, so we only
            -- check the colour channels (positions where i `mod` 4 /= 3).
            rgba <- VS.freeze (ppuFbRgba ps)
            V.any (/= 0) rgb `shouldBe` True
            VS.all (== 0) (VS.ifilter (\i _ -> i `mod` 4 /= 3) rgba) `shouldBe` True

        it "FbRgba mode writes the RGBA buffer but leaves the RGB buffer untouched" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            setFbTarget FbRgba ps
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            rgba <- framebufferRgbaBytes ps
            V.all (== 0) rgb `shouldBe` True
            not (BS.all (== 0) rgba) `shouldBe` True

    describe "sprite rendering" $ do
        it "an 8x8 sprite at x=8,y=0 renders through OBP0 over a transparent BG" $ do
            ps <- freshOn
            writeVram ps [(16, 0xFF), (17, 0xFF)]
            writeOam ps [(0, 16), (1, 16), (2, 0x01), (3, 0x00)]
            writeIORef (ppuLcdc ps) 0x93
            writeIORef (ppuObp0 ps) 0xE4
            writeIORef (ppuBgp ps) 0xE4
            advanceThroughDraw ps
            fb <- framebuffer ps
            fb V.! 8 `shouldBe` 0x03
            fb V.! 15 `shouldBe` 0x03
            fb V.! 7 `shouldBe` 0x00

        it "sprites are disabled when LCDC bit 1 is clear" $ do
            ps <- freshOn
            writeVram ps [(16, 0xFF), (17, 0xFF)]
            writeOam ps [(0, 16), (1, 16), (2, 0x01), (3, 0x00)]
            writeIORef (ppuLcdc ps) 0x91
            writeIORef (ppuObp0 ps) 0xE4
            writeIORef (ppuBgp ps) 0xE4
            advanceThroughDraw ps
            fb <- framebuffer ps
            fb V.! 8 `shouldBe` 0x00

        it "sprite priority bit hides the sprite behind BG colors 1..3" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00), (16, 0xFF), (17, 0xFF)]
            writeOam ps [(0, 16), (1, 8), (2, 0x01), (3, 0x80)]
            writeIORef (ppuLcdc ps) 0x93
            writeIORef (ppuObp0 ps) 0xE4
            writeIORef (ppuBgp ps) 0xE4
            advanceThroughDraw ps
            fb <- framebuffer ps
            fb V.! 0 `shouldBe` 0x01

        it "DMG: leftmost X wins among 3+ sprites in non-monotonic OAM order" $ do
            -- Regression for a 'span'-based stableSortByX that only partitioned the longest prefix
            -- matching 'x.X < pivot.X', so e.g. sprites in OAM order [x=10, x=14, x=8] sorted to
            -- [x=10, x=8, x=14] instead of [x=8, x=10, x=14]. With the broken sort, OAM 0 (color 1)
            -- used to win the overlap pixel; the fix restores the leftmost-X (OAM 2, color 3) winner.
            ps <- freshOn
            -- Three solid-color tiles. Each tile is two bytes per row, 8 rows = 16 bytes.
            -- Tile N starts at VRAM 16*N.
            writeVram
                ps
                [ (16, 0xFF) -- Tile 1, row 0 lo: color 1 across the row
                , (17, 0x00)
                , (32, 0x00) -- Tile 2, row 0
                , (33, 0xFF) -- Color 2
                , (48, 0xFF) -- Tile 3, row 0
                , (49, 0xFF) -- Color 3
                ]
            -- All three sprites at scanline 0. X coords chosen so they all cover screen pixel 14:
            --   OAM 0: byte1=18 -> sprite X=10, range [10,17]
            --   OAM 1: byte1=22 -> sprite X=14, range [14,21]
            --   OAM 2: byte1=16 -> sprite X=8,  range [8,15]
            writeOam
                ps
                [ (0, 16)
                , (1, 18)
                , (2, 0x01)
                , (3, 0x00)
                , (4, 16)
                , (5, 22)
                , (6, 0x02)
                , (7, 0x00)
                , (8, 16)
                , (9, 16)
                , (10, 0x03)
                , (11, 0x00)
                ]
            writeIORef (ppuLcdc ps) 0x93
            writeIORef (ppuObp0 ps) 0xE4 -- Identity
            writeIORef (ppuBgp ps) 0xE4
            advanceThroughDraw ps
            fb <- framebuffer ps
            -- Pixel 14 is covered by all three sprites. With leftmost-X priority,
            -- OAM 2 (X=8, tile 3 -> color 3) must win.
            fb V.! 14 `shouldBe` 0x03

readMode :: PpuState -> IO PpuMode
readMode ps = readIORef (ppuMode ps)

readDot :: PpuState -> IO Int
readDot ps = readIORef (ppuDot ps)

readLy :: PpuState -> IO Word8
readLy ps = readIORef (ppuLy ps)

rgbaFromRgb :: [Word8] -> [Word8]
rgbaFromRgb [] = []
rgbaFromRgb (r : g : b : rest) = r : g : b : 255 : rgbaFromRgb rest
rgbaFromRgb _ = error "RGB framebuffer length must be a multiple of 3"

pitchedRgb :: Int -> Int -> [Word8] -> [Word8]
pitchedRgb rowBytes pitch = go
  where
    padBytes = pitch - rowBytes
    go [] = []
    go xs =
        let (row, rest) = splitAt rowBytes xs
         in row <> replicate padBytes 0xAA <> go rest
