{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ocelot.PpuSpec (spec) where

import Control.Monad (forM_)
import Data.Bits ((.&.))
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
            let turnOn = do
                    ps <- initialPpu
                    write8 0xFF40 0x11 ps -- LCD off
                    write8 0xFF40 0x91 ps -- LCD on: starts the short line
                    pure ps

            -- 'initialPpu' is DMG, so these expect the DMG figures: mode 3 starts at dot 79 and the
            -- line runs 449 dots. On CGB both drop by one (78 and 448); SameBoy spends one extra dot
            -- before the post-enable line begins on DMG only.
            -- 'advance' moves 4 dots at a time, so stepping alone brackets the boundary only to
            -- 77..80. Drive the dot counter directly to pin the exact figure, otherwise 77, 79, and
            -- 80 are all indistinguishable.
            it "starts drawing at exactly dot 79 on DMG" $ do
                ps <- turnOn
                writeIORef (ppuDot ps) 78
                _ <- advance 0 ps
                m78 <- readMode ps
                ps2 <- turnOn
                writeIORef (ppuDot ps2) 78
                _ <- advance 1 ps2 -- 78 -> 79 crosses the boundary
                m79 <- readMode ps2
                (m78, m79) `shouldBe` (ModeHBlank, ModeDrawing)

            it "starts drawing one dot earlier on CGB" $ do
                -- The DMG-only extra dot; without it this and the DMG case would agree.
                ps <- initialPpu
                setCgbMode True ps
                write8 0xFF40 0x11 ps
                write8 0xFF40 0x91 ps
                writeIORef (ppuDot ps) 77
                _ <- advance 1 ps -- 77 -> 78 crosses the CGB boundary
                m <- readMode ps
                m `shouldBe` ModeDrawing

            it "runs one dot longer on DMG than on CGB" $ do
                let lineEndOn cgb = do
                        ps <- initialPpu
                        setCgbMode cgb ps
                        write8 0xFF40 0x11 ps
                        write8 0xFF40 0x91 ps
                        writeIORef (ppuDot ps) 440
                        let go !n
                                | n > 20 = pure (-1)
                                | otherwise = do
                                    ly <- readLy ps
                                    if ly == 1 then pure n else advance 1 ps >> go (n + 1)
                        go 0
                dmg <- lineEndOn False
                cgb <- lineEndOn True
                -- One extra M-cycle of stepping on DMG, i.e. 449 dots against 448.
                (dmg - cgb) `shouldBe` 1

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
                _ <- advance 18 ps -- 72 T-cycles, still short of dot 76
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
            _ <- advance ((80 + 172) `div` 4) ps
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
            _ <- advance ((80 + 172) `div` 4) ps
            fb <- framebuffer ps
            fb V.! 8 `shouldBe` 0x00

        it "sprite priority bit hides the sprite behind BG colors 1..3" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00), (16, 0xFF), (17, 0xFF)]
            writeOam ps [(0, 16), (1, 8), (2, 0x01), (3, 0x80)]
            writeIORef (ppuLcdc ps) 0x93
            writeIORef (ppuObp0 ps) 0xE4
            writeIORef (ppuBgp ps) 0xE4
            _ <- advance ((80 + 172) `div` 4) ps
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
            _ <- advance ((80 + 172) `div` 4) ps
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
