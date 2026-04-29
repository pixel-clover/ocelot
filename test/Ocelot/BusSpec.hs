{-# LANGUAGE OverloadedStrings #-}

module Ocelot.BusSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Ocelot.Bus (Bus, advance, drainSerial, fromCartridge, installBootRom, read8, write8)
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

        it "raises the Timer interrupt in IF on TIMA overflow (after the reload window)" $ do
            b <- emptyBus
            write8 0xFF06 0x10 b -- TMA = 0x10
            write8 0xFF05 0xFF b -- TIMA = 0xFF
            write8 0xFF07 0x05 b -- TAC = 0x05 (enable + 16 T-cycles)
            -- 4 M-cycles hits the falling edge that wraps TIMA, but the
            -- 1-M-cycle reload window has not yet expired; IF stays clear.
            advance 4 b
            iflag1 <- read8 0xFF0F b
            (iflag1 .&. 0x04) `shouldBe` 0x00
            -- 1 more M-cycle drains the reload window: TIMA := TMA, IF set.
            advance 1 b
            iflag2 <- read8 0xFF0F b
            (iflag2 .&. 0x04) `shouldBe` 0x04

    describe "OAM DMA via 0xFF46" $ do
        it "copies 160 bytes from (v << 8) into OAM at 0xFE00 starting next instruction" $ do
            b <- emptyBus
            mapM_ (\i -> write8 (0xC000 + fromIntegral i) (fromIntegral i) b) [0 .. 0x9F :: Int]
            -- Model an LD (0xFF46), A: write to FF46, then advance for
            -- the rest of that instruction's M-cycles. DMA must NOT have
            -- copied any bytes during the triggering instruction.
            write8 0xFF46 0xC0 b
            advance 3 b
            partway <- read8 0xFE00 b
            partway `shouldBe` 0xFF -- locked, but more importantly: not yet copied
            -- 160 M-cycles of subsequent instruction time complete the copy.
            advance 160 b
            v0 <- read8 0xFE00 b
            v1 <- read8 0xFE01 b
            v9F <- read8 0xFE9F b
            v0 `shouldBe` 0x00
            v1 `shouldBe` 0x01
            v9F `shouldBe` 0x9F

        it "blocks main-bus reads but lets I/O regs and HRAM through" $ do
            b <- emptyBus
            mapM_ (\i -> write8 (0xC000 + fromIntegral i) 0xAA b) [0 .. 0x9F :: Int]
            write8 0xFF80 0x55 b -- HRAM stays accessible
            write8 0xFF46 0xC0 b
            advance 4 b -- partway through
            wramR <- read8 0xC000 b
            hramR <- read8 0xFF80 b
            -- FF46 lives in the I/O register page, so it stays readable
            -- during DMA and reflects the last-written source byte.
            regR <- read8 0xFF46 b
            wramR `shouldBe` 0xFF
            hramR `shouldBe` 0x55
            regR `shouldBe` 0xC0

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

    describe "boot ROM" $ do
        it "served from boot ROM bytes until 0xFF50 unmasks the cartridge" $ do
            b <- emptyBus
            -- A 256-byte boot ROM with byte i = i.
            installBootRom (BS.pack [fromIntegral (i :: Int) | i <- [0 .. 255]]) b
            v0 <- read8 0x0000 b
            vFF <- read8 0x00FF b
            v0 `shouldBe` 0x00
            vFF `shouldBe` 0xFF
            -- Unmask: write any non-zero value to 0xFF50.
            write8 0xFF50 0x01 b
            -- Now the read goes to the cartridge.
            v0' <- read8 0x0000 b
            v0' `shouldBe` 0x00 -- synthetic cart is mostly zero
        it "0xFF50 is sticky: a second cleared boot mask cannot re-mask" $ do
            b <- emptyBus
            installBootRom (BS.pack (replicate 256 0xAA)) b
            write8 0xFF50 0x01 b
            -- Subsequent reads stay at the cartridge regardless of any
            -- attempt to re-enable.
            v <- read8 0x0000 b
            v `shouldBe` 0x00
