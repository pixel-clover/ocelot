{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cartridge.Mbc5Spec (spec) where

import Data.Bits ((.&.))
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
        Left e -> error ("Mbc5Spec: cartridge did not load: " ++ show e)

buildRom :: Int -> Int -> ByteString
buildRom nBanks nRamBanks =
    let romSize = nBanks * 0x4000
        ramCode :: Word8
        ramCode = case nRamBanks of
            0 -> 0x00
            4 -> 0x03
            16 -> 0x04
            _ -> error "buildRom: only 0, 4, or 16 RAM banks supported"
        romCode :: Word8
        romCode = case nBanks of
            2 -> 0x00
            4 -> 0x01
            16 -> 0x03
            128 -> 0x06
            _ -> error "buildRom: unsupported nBanks"
        v0 = V.replicate romSize 0xFF :: V.Vector Word8
        bankFiller =
            [ (b * 0x4000 + i, fromIntegral (b .&. 0xFF))
            | b <- [0 .. nBanks - 1]
            , i <- [0 .. 0x3FFF]
            ]
        title = BSC.pack "MBC5ROM"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (title `BS.append` BS.replicate 16 0)))
        cartType :: Word8
        cartType = case nRamBanks of
            0 -> 0x19
            _ -> 0x1B
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
        it "writing 0x00 to 0x2000 maps bank 0 (no quirk on MBC5)" $ do
            c <- buildCart 4 0
            write8 0x2000 0x00 c
            v <- read8 0x4000 c
            v `shouldBe` 0x00

        it "writing 0x03 to 0x2000 selects bank 3" $ do
            c <- buildCart 4 0
            write8 0x2000 0x03 c
            v <- read8 0x4000 c
            v `shouldBe` 0x03

        it "writing the high bit at 0x3000 with low byte at 0x2000 wraps mod the cart's bank count" $ do
            -- 128-bank ROM has 7 bank-select lines wired. Writing bank index 0x105 (bit 8 set)
            -- wraps mod 128 to bank 0x05. The cart fills bank N's bytes with @N & 0xFF@,
            -- so reading at $4000 returns 0x05.
            c <- buildCart 128 0
            write8 0x3000 0x01 c
            write8 0x2000 0x05 c
            v <- read8 0x4000 c
            v `shouldBe` 0x05

    describe "RAM enable and round-trip" $ do
        it "RAM is locked until 0x0A is written to 0x0000" $ do
            c <- buildCart 2 4
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

        it "after enable, RAM round-trips" $ do
            c <- buildCart 2 4
            write8 0x0000 0x0A c
            write8 0xA000 0x77 c
            v <- read8 0xA000 c
            v `shouldBe` 0x77

    describe "RAM bank switching" $ do
        it "writes to 0x4000 select the RAM bank (4 bits)" $ do
            c <- buildCart 2 4
            write8 0x0000 0x0A c
            write8 0x4000 0x00 c
            write8 0xA000 0x11 c
            write8 0x4000 0x03 c
            write8 0xA000 0x33 c
            write8 0x4000 0x00 c
            v <- read8 0xA000 c
            v `shouldBe` 0x11
