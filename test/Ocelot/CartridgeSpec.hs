{-# LANGUAGE OverloadedStrings #-}

module Ocelot.CartridgeSpec (spec) where

import Control.Exception (IOException, try)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge
import Ocelot.Cartridge.Header
import Test.Hspec

spec :: Spec
spec = do
    describe "loadRom" $ do
        it "accepts a synthetic NoMbc 32 KiB ROM" $ do
            r <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "OCELOT")
            case r of
                Right c -> do
                    hdrTitle (cartridgeHeader c) `shouldBe` "OCELOT"
                    hdrMbcKind (cartridgeHeader c) `shouldBe` NoMbc
                Left e -> expectationFailure (show e)

        it "accepts an MBC1 cartridge" $ do
            r <- loadRom (mkSyntheticRom 0x01 0x00 0x00 "MBC1ROM")
            case r of
                Right c -> hdrMbcKind (cartridgeHeader c) `shouldBe` Mbc1
                Left e -> expectationFailure (show e)

        it "rejects MBC2 cartridges as UnsupportedMbcKind" $ do
            r <- loadRom (mkSyntheticRom 0x05 0x00 0x00 "X")
            case r of
                Left (UnsupportedMbcKind Mbc2) -> pure ()
                Left other -> expectationFailure ("expected MBC2 rejection, got: " ++ show other)
                Right _ -> expectationFailure "expected MBC2 rejection, got Right"

        it "wraps header errors in HeaderParse" $ do
            r <- loadRom (BS.replicate 0x100 0)
            case r of
                Left (HeaderParse _) -> pure ()
                Left other -> expectationFailure ("expected HeaderParse, got: " ++ show other)
                Right _ -> expectationFailure "expected HeaderParse, got Right"

    describe "read8 (NoMbc)" $ do
        it "reads the entry-point bytes at 0x0100..0x0103" $ do
            Right c <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            vs <- mapM (\a -> read8 a c) [0x0100, 0x0101, 0x0102, 0x0103]
            vs `shouldBe` [0x00, 0xC3, 0x50, 0x01]

        it "reads the cartridge type byte at 0x0147" $ do
            Right c <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            v <- read8 0x0147 c
            v `shouldBe` 0x00

        it "returns 0xFF for ERAM reads when the cartridge has no RAM" $ do
            Right c <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

    describe "write8 (NoMbc)" $ do
        it "ignores writes to the ROM window" $ do
            Right c <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            write8 0x0100 0xAB c
            v <- read8 0x0100 c
            v `shouldBe` 0x00

        it "round-trips RAM writes when the cartridge has 8 KiB ERAM" $ do
            Right c <- loadRom (mkSyntheticRom 0x08 0x00 0x02 "X")
            write8 0xA000 0xAB c
            write8 0xBFFF 0xCD c
            v0 <- read8 0xA000 c
            v1 <- read8 0xBFFF c
            v0 `shouldBe` 0xAB
            v1 `shouldBe` 0xCD

        it "drops RAM writes when the cartridge has no RAM" $ do
            Right c <- loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            write8 0xA000 0xAB c
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

    describe "MBC3 RTC" $ do
        let mkMbc3Rtc = mkSyntheticRom 0x10 0x00 0x02 "MBC3RTC"
            enableRamRtc c = write8 0x0000 0x0A c
            -- Halt the RTC and zero everything so tests don't depend on wall clock.
            zeroAndHalt c = do
                enableRamRtc c
                write8 0x4000 0x08 c >> write8 0xA000 0x00 c
                write8 0x4000 0x09 c >> write8 0xA000 0x00 c
                write8 0x4000 0x0A c >> write8 0xA000 0x00 c
                write8 0x4000 0x0B c >> write8 0xA000 0x00 c
                write8 0x4000 0x0C c >> write8 0xA000 0x40 c
            latch c = write8 0x6000 0x00 c >> write8 0x6000 0x01 c

        it "round-trips RTC writes through latch + read" $ do
            Right c <- loadRom mkMbc3Rtc
            zeroAndHalt c
            write8 0x4000 0x08 c >> write8 0xA000 0x2A c
            write8 0x4000 0x09 c >> write8 0xA000 0x05 c
            write8 0x4000 0x0A c >> write8 0xA000 0x11 c
            write8 0x4000 0x0B c >> write8 0xA000 0x33 c
            latch c
            sec <- (write8 0x4000 0x08 c >> read8 0xA000 c)
            mn <- (write8 0x4000 0x09 c >> read8 0xA000 c)
            hr <- (write8 0x4000 0x0A c >> read8 0xA000 c)
            dl <- (write8 0x4000 0x0B c >> read8 0xA000 c)
            sec `shouldBe` 0x2A
            mn `shouldBe` 0x05
            hr `shouldBe` 0x11
            dl `shouldBe` 0x33

        it "halt freezes the seconds register across latches" $ do
            Right c <- loadRom mkMbc3Rtc
            zeroAndHalt c
            write8 0x4000 0x08 c >> write8 0xA000 0x07 c
            latch c
            v1 <- (write8 0x4000 0x08 c >> read8 0xA000 c)
            latch c
            v2 <- (write8 0x4000 0x08 c >> read8 0xA000 c)
            v1 `shouldBe` 0x07
            v2 `shouldBe` 0x07

        it "DH bit 0 carries the day-high bit" $ do
            Right c <- loadRom mkMbc3Rtc
            zeroAndHalt c
            write8 0x4000 0x0C c >> write8 0xA000 0x41 c
            latch c
            dh <- (write8 0x4000 0x0C c >> read8 0xA000 c)
            dh `shouldBe` 0x41

        it "DH bit 7 (day carry) is sticky until cleared" $ do
            Right c <- loadRom mkMbc3Rtc
            zeroAndHalt c
            write8 0x4000 0x0C c >> write8 0xA000 0xC0 c
            latch c
            dh1 <- (write8 0x4000 0x0C c >> read8 0xA000 c)
            write8 0x4000 0x0C c >> write8 0xA000 0x40 c
            latch c
            dh2 <- (write8 0x4000 0x0C c >> read8 0xA000 c)
            dh1 `shouldBe` 0xC0
            dh2 `shouldBe` 0x40

        it "RAM/RTC reads return 0xFF when the enable register is 0" $ do
            Right c <- loadRom mkMbc3Rtc
            write8 0x4000 0x08 c
            v <- read8 0xA000 c
            v `shouldBe` 0xFF

        it "RAM bank 0..3 still works with RTC mapping mode" $ do
            Right c <- loadRom mkMbc3Rtc
            enableRamRtc c
            write8 0x4000 0x00 c
            write8 0xA000 0xAB c
            v <- read8 0xA000 c
            v `shouldBe` 0xAB

        it "extractSave/loadSave round-trips RAM and RTC across cart instances" $ do
            Right c <- loadRom mkMbc3Rtc
            -- Stash a RAM byte and a halted RTC value.
            enableRamRtc c
            write8 0x4000 0x00 c
            write8 0xA000 0x77 c
            zeroAndHalt c
            write8 0x4000 0x09 c >> write8 0xA000 0x12 c
            write8 0x4000 0x0B c >> write8 0xA000 0x05 c
            latch c
            blob <- extractSave c
            BS.length blob `shouldBe` 8 * 1024 + 48
            -- Load into a fresh cart and verify both RAM and RTC came back.
            Right c2 <- loadRom mkMbc3Rtc
            loadSave blob c2
            enableRamRtc c2
            write8 0x4000 0x00 c2
            ramByte <- read8 0xA000 c2
            latch c2
            mn <- (write8 0x4000 0x09 c2 >> read8 0xA000 c2)
            dl <- (write8 0x4000 0x0B c2 >> read8 0xA000 c2)
            ramByte `shouldBe` 0x77
            mn `shouldBe` 0x12
            dl `shouldBe` 0x05

        it "extractSave for a non-RTC cart has no suffix" $ do
            Right c <- loadRom (mkSyntheticRom 0x13 0x00 0x02 "MBC3RAM")
            blob <- extractSave c
            BS.length blob `shouldBe` 8 * 1024

        it "loadSave accepts an old RAM-only .sav (no RTC suffix)" $ do
            Right c <- loadRom mkMbc3Rtc
            let ramOnly = BS.replicate (8 * 1024) 0xAB
            loadSave ramOnly c
            enableRamRtc c
            write8 0x4000 0x00 c
            v <- read8 0xA000 c
            v `shouldBe` 0xAB

    describe "external/gb-test-roms" $ do
        it "loads cpu_instrs.gb (MBC1, 64 KiB) cleanly" $ do
            mb <- tryReadFile "external/gb-test-roms/cpu_instrs/cpu_instrs.gb"
            case mb of
                Nothing -> pendingWith "external/gb-test-roms submodule not initialized"
                Just bytes -> do
                    r <- loadRom bytes
                    case r of
                        Right c -> do
                            hdrMbcKind (cartridgeHeader c) `shouldBe` Mbc1
                            hdrRomBytes (cartridgeHeader c) `shouldBe` 64 * 1024
                        Left e -> expectationFailure ("expected Right, got: " ++ show e)

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile path = do
    r <- try (BS.readFile path) :: IO (Either IOException ByteString)
    pure (either (const Nothing) Just r)

mkSyntheticRom :: Word8 -> Word8 -> Word8 -> String -> ByteString
mkSyntheticRom cartType romCode ramCode title =
    let romSize = 8 * 1024 `shiftL` fromIntegral romCode
        v0 = V.replicate romSize 0 :: V.Vector Word8
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
            , (0x0147, cartType)
            , (0x0148, romCode)
            , (0x0149, ramCode)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        v1 = v0 V.// fields
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.pack (V.toList (v1 V.// [(0x014D, cs)]))
