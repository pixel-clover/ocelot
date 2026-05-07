{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cartridge.Mbc1Spec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge (Cartridge, loadRom, read8, resetMbc, write8)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import Test.Hspec

buildCart :: Int -> Int -> IO Cartridge
buildCart nBanks nRamBanks = do
    r <- loadRom (buildRom nBanks nRamBanks)
    case r of
        Right c -> pure c
        Left e -> error ("Mbc1Spec: cartridge did not load: " ++ show e)

buildRom :: Int -> Int -> ByteString
buildRom nBanks nRamBanks =
    let romSize = nBanks * 0x4000
        ramCode = case nRamBanks of
            0 -> 0x00
            1 -> 0x02
            4 -> 0x03
            _ -> error "buildRom: only 0, 1, or 4 RAM banks supported"
        romCode :: Word8
        romCode = case nBanks of
            2 -> 0x00
            4 -> 0x01
            8 -> 0x02
            _ -> error "buildRom: nBanks must be 2, 4, or 8"
        v0 = V.replicate romSize 0xFF :: V.Vector Word8
        bankFiller =
            [ (b * 0x4000 + i, fromIntegral b)
            | b <- [0 .. nBanks - 1]
            , i <- [0 .. 0x3FFF]
            ]
        title = BSC.pack "MBC1ROM"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (title `BS.append` BS.replicate 16 0)))
        cartType :: Word8
        cartType = if nRamBanks == 0 then 0x01 else 0x03
        headerFields =
            [ (0x0146, 0x00)
            , (0x0147, cartType)
            , (0x0148, romCode)
            , (0x0149, ramCode)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        v1 = v0 V.// (bankFiller ++ headerFields)
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.pack (V.toList (v1 V.// [(0x014D, cs)]))

spec :: Spec
spec = do
    describe "default state" $ do
        it "after load, 0x0000 reads from bank 0 and 0x4000 reads from bank 1" $ do
            c <- buildCart 4 0
            v0 <- read8 0x0000 c
            v0 `shouldBe` 0x00
            v1 <- read8 0x4000 c
            v1 `shouldBe` 0x01
            v7 <- read8 0x7FFF c
            v7 `shouldBe` 0x01

    describe "ROM bank switching" $ do
        it "writing 0x02 to 0x2000 selects ROM bank 2 at 0x4000-0x7FFF" $ do
            c <- buildCart 4 0
            write8 0x2000 0x02 c
            v <- read8 0x4000 c
            v `shouldBe` 0x02
            v7 <- read8 0x7FFF c
            v7 `shouldBe` 0x02

        it "writing 0x00 to 0x2000 still maps bank 1 (the 0/0x20/0x40/0x60 quirk)" $ do
            c <- buildCart 4 0
            write8 0x2000 0x00 c
            v <- read8 0x4000 c
            v `shouldBe` 0x01

    describe "RAM enable and round-trip" $ do
        it "RAM reads return 0xFF until RAM is enabled" $ do
            c <- buildCart 2 1
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

        it "after enable, RAM round-trips a byte" $ do
            c <- buildCart 2 1
            write8 0x0000 0x0A c
            write8 0xA000 0x42 c
            v <- read8 0xA000 c
            v `shouldBe` 0x42

        it "writing a non-0x?A value to 0x0000 disables RAM" $ do
            c <- buildCart 2 1
            write8 0x0000 0x0A c
            write8 0xA000 0x55 c
            write8 0x0000 0x00 c
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

        it "resetMbc resets banking registers while preserving RAM bytes" $ do
            c <- buildCart 4 1
            write8 0x0000 0x0A c
            write8 0x2000 0x02 c
            write8 0xA000 0x42 c
            resetMbc c
            rom <- read8 0x4000 c
            rom `shouldBe` 0x01
            locked <- read8 0xA000 c
            locked `shouldBe` 0xFF
            write8 0x0000 0x0A c
            ram <- read8 0xA000 c
            ram `shouldBe` 0x42

    describe "RAM-banking mode" $ do
        it "in RAM mode, writes to 0x4000 select the RAM bank" $ do
            c <- buildCart 2 4
            write8 0x0000 0x0A c
            write8 0x6000 0x01 c
            write8 0x4000 0x00 c
            write8 0xA000 0x10 c
            write8 0x4000 0x01 c
            write8 0xA000 0x20 c
            write8 0x4000 0x00 c
            v <- read8 0xA000 c
            v `shouldBe` 0x10
