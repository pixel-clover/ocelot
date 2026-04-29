{-# LANGUAGE OverloadedStrings #-}

module Ocelot.BusSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Ocelot.Bus (Bus, advance, drainSerial, fromCartridge, read8, write8)
import Ocelot.Cartridge (loadRom)
import Ocelot.Testing (synthNoMbcRom)
import Test.Hspec

emptyBus :: IO Bus
emptyBus = do
    let rom = synthNoMbcRom BS.empty
    result <- loadRom rom
    case result of
        Right c -> fromCartridge c
        Left e -> error ("test setup: cartridge did not load: " ++ show e)

spec :: Spec
spec = do
    describe "address routing" $ do
        it "WRAM round-trips a byte at 0xC000" $ do
            b <- emptyBus
            write8 0xC000 0xAB b
            v <- read8 0xC000 b
            v `shouldBe` 0xAB

        it "echo RAM at 0xE000 mirrors WRAM at 0xC000" $ do
            b <- emptyBus
            write8 0xC000 0xCD b
            v <- read8 0xE000 b
            v `shouldBe` 0xCD

        it "echo writes at 0xE000 are visible at 0xC000" $ do
            b <- emptyBus
            write8 0xE000 0x42 b
            v <- read8 0xC000 b
            v `shouldBe` 0x42

        it "HRAM round-trips a byte at 0xFF80" $ do
            b <- emptyBus
            write8 0xFF80 0x33 b
            v <- read8 0xFF80 b
            v `shouldBe` 0x33

        it "OAM round-trips a byte at 0xFE00" $ do
            b <- emptyBus
            write8 0xFE00 0x77 b
            v <- read8 0xFE00 b
            v `shouldBe` 0x77

        it "IE byte at 0xFFFF round-trips" $ do
            b <- emptyBus
            write8 0xFFFF 0x1F b
            v <- read8 0xFFFF b
            v `shouldBe` 0x1F

        it "unusable region 0xFEA0..0xFEFF returns 0xFF" $ do
            b <- emptyBus
            v0 <- read8 0xFEA0 b
            v1 <- read8 0xFEFF b
            v0 `shouldBe` 0xFF
            v1 `shouldBe` 0xFF

        it "ROM writes are forwarded to the cartridge (NoMbc ignores them)" $ do
            b <- emptyBus
            write8 0x0000 0x55 b
            v <- read8 0x0000 b
            v `shouldBe` 0x00

    describe "serial-port capture" $ do
        it "writing 0x81 to SC after staging SB latches a byte to drainSerial" $ do
            b <- emptyBus
            write8 0xFF01 0x68 b
            write8 0xFF02 0x81 b
            ser <- drainSerial b
            ser `shouldBe` [0x68]

        it "captures multiple bytes in order" $ do
            b <- emptyBus
            mapM_
                (\ch -> write8 0xFF01 ch b >> write8 0xFF02 0x81 b)
                [0x48, 0x49, 0x21]
            ser <- drainSerial b
            ser `shouldBe` [0x48, 0x49, 0x21]

        it "clears the SC start bit after capture" $ do
            b <- emptyBus
            write8 0xFF01 0x41 b
            write8 0xFF02 0x81 b
            v <- read8 0xFF02 b
            (v `mod` 0x80) `shouldBe` 0x01

        it "writes to SC without bit 7 set do not capture" $ do
            b <- emptyBus
            write8 0xFF01 0x42 b
            write8 0xFF02 0x01 b
            ser <- drainSerial b
            ser `shouldBe` []

    describe "advance" $ do
        it "ticks the divider on each call" $ do
            b <- emptyBus
            advance 64 b
            v <- read8 0xFF04 b
            v `shouldBe` 0x01

        it "raises the Timer interrupt in IF on TIMA overflow" $ do
            b <- emptyBus
            write8 0xFF06 0x10 b
            write8 0xFF05 0xFF b
            write8 0xFF07 0x05 b
            advance 4 b
            iflag <- read8 0xFF0F b
            (iflag .&. 0x04) `shouldBe` 0x04

    describe "OAM DMA via 0xFF46" $ do
        it "copies 160 bytes from (v << 8) into OAM at 0xFE00" $ do
            b <- emptyBus
            mapM_ (\i -> write8 (0xC000 + fromIntegral i) (fromIntegral i) b) [0 .. 0x9F :: Int]
            write8 0xFF46 0xC0 b
            v0 <- read8 0xFE00 b
            v1 <- read8 0xFE01 b
            v9F <- read8 0xFE9F b
            v0 `shouldBe` 0x00
            v1 `shouldBe` 0x01
            v9F `shouldBe` 0x9F

    describe "joypad" $ do
        it "0xFF00 reads return 0xCF when no buttons are pressed" $ do
            b <- emptyBus
            v <- read8 0xFF00 b
            v `shouldBe` 0xCF

        it "0xFF00 row-select bits round-trip" $ do
            b <- emptyBus
            write8 0xFF00 0x10 b
            v <- read8 0xFF00 b
            v `shouldBe` 0xDF
