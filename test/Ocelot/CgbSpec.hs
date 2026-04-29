{-# LANGUAGE OverloadedStrings #-}

{- | CGB foundation: VRAM/WRAM banking and CGB palette register I/O.

These tests don't yet cover CGB-specific rendering (tile attributes,
RGB555 palettes feeding the framebuffer); they verify only the bus and
PPU register plumbing for VBK, BCPS\/BCPD, OCPS\/OCPD, WBK, and KEY1.
-}
module Ocelot.CgbSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
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
