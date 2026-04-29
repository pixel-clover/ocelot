{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cartridge.Mbc3Spec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge (Cartridge, loadRom, read8, write8)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import Test.Hspec

buildCart :: Int -> Int -> IO Cartridge
buildCart nBanks nRamBanks = do
    r <- loadRom (buildRom nBanks nRamBanks)
    case r of
        Right c -> pure c
        Left e -> error ("Mbc3Spec: cartridge did not load: " ++ show e)

buildRom :: Int -> Int -> ByteString
buildRom nBanks nRamBanks =
    let romSize = nBanks * 0x4000
        ramCode :: Word8
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
            16 -> 0x03
            _ -> error "buildRom: nBanks must be 2, 4, 8, or 16"
        v0 = V.replicate romSize 0xFF :: V.Vector Word8
        bankFiller =
            [ (b * 0x4000 + i, fromIntegral b)
            | b <- [0 .. nBanks - 1]
            , i <- [0 .. 0x3FFF]
            ]
        title = BSC.pack "MBC3ROM"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (title `BS.append` BS.replicate 16 0)))
        cartType :: Word8
        cartType = case nRamBanks of
            0 -> 0x11
            _ -> 0x13
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
        it "0x4000 reads from bank 1 by default" $ do
            c <- buildCart 4 0
            v <- read8 0x4000 c
            v `shouldBe` 0x01

    describe "ROM bank switching" $ do
        it "writing 0x05 to 0x2000 selects bank 5" $ do
            c <- buildCart 16 0
            write8 0x2000 0x05 c
            v <- read8 0x4000 c
            v `shouldBe` 0x05

        it "writing 0x00 to 0x2000 still maps bank 1 (MBC3 0->1 quirk)" $ do
            c <- buildCart 4 0
            write8 0x2000 0x00 c
            v <- read8 0x4000 c
            v `shouldBe` 0x01

        it "supports 7-bit ROM bank values (up to 127)" $ do
            c <- buildCart 4 0
            write8 0x2000 0x7F c
            write8 0x2000 0x02 c
            v <- read8 0x4000 c
            v `shouldBe` 0x02

    describe "RAM enable and round-trip" $ do
        it "RAM is locked until 0x0A is written to 0x0000" $ do
            c <- buildCart 2 1
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

        it "after enable, RAM round-trips a byte" $ do
            c <- buildCart 2 1
            write8 0x0000 0x0A c
            write8 0xA000 0x66 c
            v <- read8 0xA000 c
            v `shouldBe` 0x66

    describe "RAM bank switching" $ do
        it "writes to 0x4000 select RAM bank when value < 4" $ do
            c <- buildCart 2 4
            write8 0x0000 0x0A c
            write8 0x4000 0x00 c
            write8 0xA000 0x10 c
            write8 0x4000 0x02 c
            write8 0xA000 0x20 c
            write8 0x4000 0x00 c
            v <- read8 0xA000 c
            v `shouldBe` 0x10

    describe "RTC stub" $ do
        it "reads from RTC selector range return 0" $ do
            c <- buildCart 2 1
            write8 0x0000 0x0A c
            write8 0x4000 0x08 c
            v <- read8 0xA000 c
            v `shouldBe` 0x00
