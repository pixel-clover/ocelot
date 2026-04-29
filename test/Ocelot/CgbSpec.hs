{-# LANGUAGE OverloadedStrings #-}

{- | CGB foundation: VRAM/WRAM banking and CGB palette register I/O.

These tests don't yet cover CGB-specific rendering (tile attributes,
RGB555 palettes feeding the framebuffer); they verify only the bus and
PPU register plumbing for VBK, BCPS\/BCPD, OCPS\/OCPD, WBK, and KEY1.
-}
module Ocelot.CgbSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (writeIORef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import qualified Ocelot.Ppu as Ppu
import Test.Hspec

mkCgbRom :: BS.ByteString
mkCgbRom = mkRomWith 0x80 -- DmgAndCgb

mkDmgRom :: BS.ByteString
mkDmgRom = mkRomWith 0x00

mkRomWith :: Word8 -> BS.ByteString
mkRomWith cgbFlag =
    let romSize = 32 * 1024
        v0 = V.replicate romSize 0 :: V.Vector Word8
        title = "CGB"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (BSC.pack title `BS.append` BS.replicate 16 0)))
        fields =
            [ (0x0100, 0x00)
            , (0x0101, 0xC3)
            , (0x0102, 0x50)
            , (0x0103, 0x01)
            , (0x0146, 0x00)
            , (0x0147, 0x00)
            , (0x0148, 0x00)
            , (0x0149, 0x00)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        -- CGB flag goes last so it wins over the title byte at 0x143.
        v1 = v0 V.// fields V.// [(0x0143, cgbFlag)]
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.take 0x14D body0 <> BS.singleton cs <> BS.drop 0x14E body0

mkBus :: BS.ByteString -> IO Bus.Bus
mkBus rom = do
    Right cart <- Cartridge.loadRom rom
    Bus.fromCartridge cart

spec :: Spec
spec = do
    describe "CGB cartridge detection" $ do
        it "DmgAndCgb cart sets busCgb" $ do
            b <- mkBus mkCgbRom
            Bus.busCgb b `shouldBe` True

        it "DmgOnly cart leaves busCgb False" $ do
            b <- mkBus mkDmgRom
            Bus.busCgb b `shouldBe` False

    describe "VRAM banking via VBK (0xFF4F)" $ do
        it "switches the active 8 KiB bank for 0x8000-0x9FFF" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4F 0x00 b
            Bus.write8 0x8000 0xAA b
            Bus.write8 0xFF4F 0x01 b
            Bus.write8 0x8000 0xBB b
            -- Reads route through the active bank.
            Bus.write8 0xFF4F 0x00 b
            v0 <- Bus.read8 0x8000 b
            Bus.write8 0xFF4F 0x01 b
            v1 <- Bus.read8 0x8000 b
            v0 `shouldBe` 0xAA
            v1 `shouldBe` 0xBB

        it "VBK reads back as the bank bit OR 0xFE" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4F 0x01 b
            v <- Bus.read8 0xFF4F b
            v `shouldBe` 0xFF
            Bus.write8 0xFF4F 0x00 b
            v0 <- Bus.read8 0xFF4F b
            v0 `shouldBe` 0xFE

    describe "WRAM banking via WBK (0xFF70)" $ do
        it "lower 4 KiB always maps to bank 0; upper 4 KiB switches" $ do
            b <- mkBus mkCgbRom
            -- Write a marker at 0xC000 (lower bank, never switches).
            Bus.write8 0xC000 0x11 b
            -- Bank 1 (default): write a different byte at 0xD000.
            Bus.write8 0xFF70 0x01 b
            Bus.write8 0xD000 0xA1 b
            -- Bank 2: distinct byte.
            Bus.write8 0xFF70 0x02 b
            Bus.write8 0xD000 0xA2 b
            -- Lower bank still has its marker regardless of WBK.
            v0 <- Bus.read8 0xC000 b
            v0 `shouldBe` 0x11
            -- Switching back to bank 1 sees A1, bank 2 sees A2.
            Bus.write8 0xFF70 0x01 b
            v1 <- Bus.read8 0xD000 b
            Bus.write8 0xFF70 0x02 b
            v2 <- Bus.read8 0xD000 b
            v1 `shouldBe` 0xA1
            v2 `shouldBe` 0xA2

        it "WBK = 0 acts as bank 1" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF70 0x01 b
            Bus.write8 0xD000 0x77 b
            Bus.write8 0xFF70 0x00 b
            v <- Bus.read8 0xD000 b
            v `shouldBe` 0x77

        it "WBK is ignored on a DMG-only cart" $ do
            b <- mkBus mkDmgRom
            Bus.write8 0xFF70 0x05 b
            v <- Bus.read8 0xFF70 b
            v `shouldBe` 0xFF

    describe "CGB BG palette RAM (0xFF68 / 0xFF69)" $ do
        it "BCPD writes land in palette RAM at the BCPS index" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0x05 b
            Bus.write8 0xFF69 0x42 b
            Bus.write8 0xFF68 0x05 b
            v <- Bus.read8 0xFF69 b
            v `shouldBe` 0x42

        it "BCPS auto-increment advances on write when bit 7 is set" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0x80 b -- index 0, auto-increment
            Bus.write8 0xFF69 0x11 b
            Bus.write8 0xFF69 0x22 b
            Bus.write8 0xFF69 0x33 b
            -- Index should now be 0x83 (auto-inc bit kept, low 6 bits = 3).
            ix <- Bus.read8 0xFF68 b
            ix `shouldBe` 0x83
            -- Walk the entries back at offsets 0..2.
            Bus.write8 0xFF68 0x00 b
            v0 <- Bus.read8 0xFF69 b
            Bus.write8 0xFF68 0x01 b
            v1 <- Bus.read8 0xFF69 b
            Bus.write8 0xFF68 0x02 b
            v2 <- Bus.read8 0xFF69 b
            (v0, v1, v2) `shouldBe` (0x11, 0x22, 0x33)

        it "auto-increment wraps at offset 0x3F" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0xBF b -- index 0x3F, auto-increment
            Bus.write8 0xFF69 0xFE b
            ix <- Bus.read8 0xFF68 b
            ix `shouldBe` 0x80 -- wrapped to 0, auto-inc preserved
    describe "CGB OBJ palette RAM (0xFF6A / 0xFF6B)" $ do
        it "OCPD writes land in OBJ palette RAM" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF6A 0x80 b
            Bus.write8 0xFF6B 0xAA b
            Bus.write8 0xFF6B 0xBB b
            Bus.write8 0xFF6A 0x00 b
            v0 <- Bus.read8 0xFF6B b
            Bus.write8 0xFF6A 0x01 b
            v1 <- Bus.read8 0xFF6B b
            v0 `shouldBe` 0xAA
            v1 `shouldBe` 0xBB

    describe "KEY1 (0xFF4D)" $ do
        it "round-trips bit 0; bit 7 reads as 0 (always single-speed)" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4D 0x01 b
            v <- Bus.read8 0xFF4D b
            v `shouldBe` 0x7F -- bit 0 set + bits 1..6 read as 1, bit 7 = 0
        it "is 0xFF on a DMG-only cart" $ do
            b <- mkBus mkDmgRom
            v <- Bus.read8 0xFF4D b
            v `shouldBe` 0xFF

    describe "CGB BG rendering" $ do
        it "draws BG palette 0 colors when the attribute byte selects palette 0" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Tile 0 row 0: striped (color indices 0,1,0,1,...)
            -- Bytes: low=0x55, high=0x00 -> bits select color 1 every other px.
            MV.write (Ppu.ppuVram ps) 0 0x55
            MV.write (Ppu.ppuVram ps) 1 0x00
            -- Tilemap entry at 0x9800 = tile 0 (already 0).
            -- Bank 1 attribute at the same offset = palette 0, no flips, bank 0.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x00
            -- Palette 0 color 0 = pure red (RGB555 = 0x001F), color 1 = pure
            -- green (RGB555 = 0x03E0). Encoded little-endian.
            mapM_
                (\(i, v) -> MV.write (Ppu.ppuBgPalRam ps) i v)
                [(0, 0x1F), (1, 0x00), (2, 0xE0), (3, 0x03)]
            -- LCDC: enable LCD + BG; BG tile data unsigned mode; 8x8 sprites.
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            -- Run one full scanline through OAM scan + drawing + HBlank.
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Pixel 0 was bit-7 of (low=0x55,high=0x00) = 0; color 0 -> red.
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0xFF, 0x00, 0x00)
            -- Pixel 1 bit-6: low=1, high=0 -> color 1 -> green.
            (rgb V.! 3, rgb V.! 4, rgb V.! 5) `shouldBe` (0x00, 0xFF, 0x00)

        it "DMG cart still produces shade-palette RGB output" $ do
            b <- mkBus mkDmgRom
            let ps = Bus.busPpu b
            MV.write (Ppu.ppuVram ps) 0 0xFF
            MV.write (Ppu.ppuVram ps) 1 0x00
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- DMG shade 1 = (0x88, 0xC0, 0x70) per the standard palette.
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x88, 0xC0, 0x70)

        it "respects the CGB tile-data bank (attribute bit 3)" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Bank 0 tile 0: all zeros (color 0 everywhere).
            -- Bank 1 tile 0: 0xFF/0x00 -> color 1 across the whole row.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0) 0xFF
            MV.write (Ppu.ppuVram ps) (0x2000 + 1) 0x00
            -- Tilemap entry: tile 0; attr selects palette 0, bank 1 (bit 3 set).
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x08
            mapM_
                (\(i, v) -> MV.write (Ppu.ppuBgPalRam ps) i v)
                [(0, 0x00), (1, 0x00), (2, 0xE0), (3, 0x03)]
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x00, 0xFF, 0x00)

        it "horizontal-flip attribute reverses pixel order" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Tile row: low=0x80 (only bit 7 set) -> color 1 only at pixel 0.
            MV.write (Ppu.ppuVram ps) 0 0x80
            MV.write (Ppu.ppuVram ps) 1 0x00
            -- Attribute: palette 0, hflip set (bit 5).
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x20
            -- Palette 0: color 0 = white, color 1 = red.
            mapM_
                (\(i, v) -> MV.write (Ppu.ppuBgPalRam ps) i v)
                [(0, 0xFF), (1, 0x7F), (2, 0x1F), (3, 0x00)]
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Without hflip the red pixel would be at pixel 0; with hflip
            -- it should be at pixel 7.
            let pixelRgb i = (rgb V.! (i * 3), rgb V.! (i * 3 + 1), rgb V.! (i * 3 + 2))
            pixelRgb 0 `shouldBe` (0xFF, 0xFF, 0xFF)
            pixelRgb 7 `shouldBe` (0xFF, 0x00, 0x00)
