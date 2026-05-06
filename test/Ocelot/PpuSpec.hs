{-# LANGUAGE OverloadedStrings #-}

module Ocelot.PpuSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
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

        it "RGBA byte snapshots expand RGB pixels with opaque alpha" $ do
            ps <- freshOn
            writeVram ps [(0, 0xFF), (1, 0x00)]
            _ <- advance ((80 + 172) `div` 4) ps
            rgb <- framebufferRgb ps
            rgbaBytes <- framebufferRgbaBytes ps
            BS.unpack rgbaBytes `shouldBe` rgbaFromRgb (V.toList rgb)

    describe "register I/O" $ do
        it "STAT read returns mode bits 0..1 from the current mode" $ do
            ps <- freshOn
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
            _ <- advance ((80 + 172) `div` 4) ps
            fb <- framebuffer ps
            fb V.! 0 `shouldBe` 0x01

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
