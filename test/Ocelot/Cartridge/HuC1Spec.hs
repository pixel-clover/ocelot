{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cartridge.HuC1Spec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge (Cartridge, loadRom, read8, resetMbc, write8)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import Test.Hspec

{- | A 64 KiB HuC1 cart whose every ROM bank is filled with its own bank number,
so a read from @0x4000@ names the bank that is mapped there. RAM is 32 KiB, which
is four banks, because the HuC1 RAM bank register is two bits wide.
-}
buildCart :: IO Cartridge
buildCart = do
    r <- loadRom buildRom
    case r of
        Right c -> pure c
        Left e -> error ("HuC1Spec: cartridge did not load: " ++ show e)

buildRom :: ByteString
buildRom =
    let nBanks = 4
        v0 = V.replicate (nBanks * 0x4000) 0xFF :: V.Vector Word8
        bankFiller =
            [ (b * 0x4000 + i, fromIntegral b)
            | b <- [0 .. nBanks - 1]
            , i <- [0 .. 0x3FFF]
            ]
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (BSC.pack "HUC1ROM" `BS.append` BS.replicate 16 0)))
        headerFields =
            [ (0x0146, 0x00)
            , (0x0147, 0xFF) -- HuC1 with RAM and battery
            , (0x0148, 0x01) -- 64 KiB
            , (0x0149, 0x03) -- 32 KiB RAM, four banks
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        v1 = v0 V.// (bankFiller ++ headerFields)
        cs = expectedHeaderChecksum (BS.pack (V.toList v1))
     in BS.pack (V.toList (v1 V.// [(0x014D, cs)]))

-- | Select RAM mode. Any low nibble other than 0xE does it; 0xA matches other MBCs.
selectRam :: Cartridge -> IO ()
selectRam = write8 0x0000 0x0A

-- | Select IR mode, which is what hides RAM again.
selectIr :: Cartridge -> IO ()
selectIr = write8 0x0000 0x0E

spec :: Spec
spec = do
    describe "ROM banking" $ do
        it "maps bank 0 at 0x4000 after load, because HuC1 does not translate 0 to 1" $ do
            -- This is the headline difference from MBC1, whose bank register reads
            -- back as 1 from reset and can never select bank 0 at 0x4000.
            c <- buildCart
            low <- read8 0x0000 c
            low `shouldBe` 0x00
            high <- read8 0x4000 c
            high `shouldBe` 0x00

        it "writing 0x02 to 0x2000 maps ROM bank 2 across the whole window" $ do
            c <- buildCart
            write8 0x2000 0x02 c
            v <- read8 0x4000 c
            v `shouldBe` 0x02
            vEnd <- read8 0x7FFF c
            vEnd `shouldBe` 0x02

        it "writing 0x00 to 0x2000 maps bank 0, not bank 1" $ do
            c <- buildCart
            write8 0x2000 0x03 c
            write8 0x2000 0x00 c
            v <- read8 0x4000 c
            v `shouldBe` 0x00

        it "keeps only the low six bits of the bank write" $ do
            -- 0x42 is 0b0100_0010, so the 6-bit register takes 0x02, and the
            -- four-bank cart masks that to bank 2.
            c <- buildCart
            write8 0x2000 0x42 c
            v <- read8 0x4000 c
            v `shouldBe` 0x02

        it "leaves 0x0000-0x3FFF mapped to bank 0 regardless of the bank register" $ do
            c <- buildCart
            write8 0x2000 0x03 c
            v <- read8 0x3FFF c
            v `shouldBe` 0x00

    describe "IR mode and RAM gating" $ do
        it "reads 0xC0 from the RAM window while IR mode is selected" $ do
            -- Not 0xFF: HuC1 answers IR-mode reads, and Ocelot approximates a
            -- receiver with no data present.
            c <- buildCart
            v <- read8 0xA000 c
            v `shouldBe` 0xC0

        it "round-trips a RAM byte once RAM mode is selected" $ do
            c <- buildCart
            selectRam c
            write8 0xA000 0x42 c
            v <- read8 0xA000 c
            v `shouldBe` 0x42

        it "hides RAM again when IR mode is reselected" $ do
            c <- buildCart
            selectRam c
            write8 0xA000 0x42 c
            selectIr c
            v <- read8 0xA000 c
            v `shouldBe` 0xC0

        it "drops RAM writes made while IR mode is selected" $ do
            c <- buildCart
            selectRam c
            write8 0xA000 0x11 c
            selectIr c
            write8 0xA000 0x99 c
            selectRam c
            v <- read8 0xA000 c
            v `shouldBe` 0x11

    describe "RAM banking" $ do
        it "keeps four independent banks selected by the low two bits of 0x4000" $ do
            c <- buildCart
            selectRam c
            mapM_
                (\b -> write8 0x4000 b c >> write8 0xA000 (0x10 * (b + 1)) c)
                [0 .. 3]
            vs <- mapM (\b -> write8 0x4000 b c >> read8 0xA000 c) [0 .. 3]
            vs `shouldBe` [0x10, 0x20, 0x30, 0x40]

        it "masks the RAM bank write to two bits" $ do
            c <- buildCart
            selectRam c
            write8 0x4000 0x00 c
            write8 0xA000 0x7B c
            -- 0x04 masks to bank 0, so this must land on the byte just written.
            write8 0x4000 0x04 c
            v <- read8 0xA000 c
            v `shouldBe` 0x7B

    describe "unmapped register window" $ do
        it "ignores writes to 0x6000-0x7FFF" $ do
            c <- buildCart
            write8 0x2000 0x02 c
            selectRam c
            write8 0xA000 0x5A c
            write8 0x6000 0x01 c
            write8 0x7FFF 0xFF c
            bank <- read8 0x4000 c
            bank `shouldBe` 0x02
            ram <- read8 0xA000 c
            ram `shouldBe` 0x5A

    describe "resetMbc" $ do
        it "returns to bank 0 with RAM hidden while keeping RAM contents" $ do
            c <- buildCart
            selectRam c
            write8 0x2000 0x03 c
            write8 0xA000 0x42 c
            resetMbc c
            bank <- read8 0x4000 c
            bank `shouldBe` 0x00
            hidden <- read8 0xA000 c
            hidden `shouldBe` 0xC0
            selectRam c
            kept <- read8 0xA000 c
            kept `shouldBe` 0x42
