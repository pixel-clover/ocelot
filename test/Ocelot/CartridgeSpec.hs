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
            case loadRom (mkSyntheticRom 0x00 0x00 0x00 "OCELOT") of
                Right c -> do
                    hdrTitle (cartridgeHeader c) `shouldBe` "OCELOT"
                    hdrMbcKind (cartridgeHeader c) `shouldBe` NoMbc
                Left e -> expectationFailure (show e)

        it "rejects MBC1 cartridges as UnsupportedMbcKind" $ do
            loadRom (mkSyntheticRom 0x01 0x00 0x00 "X")
                `shouldBe` Left (UnsupportedMbcKind Mbc1)

        it "wraps header errors in HeaderParse" $ do
            case loadRom (BS.replicate 0x100 0) of
                Left (HeaderParse _) -> pure ()
                other -> expectationFailure ("expected HeaderParse, got: " ++ show other)

    describe "read8 (NoMbc)" $ do
        it "reads the entry-point bytes at 0x0100..0x0103" $ do
            let Right c = loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            map (`read8` c) [0x0100, 0x0101, 0x0102, 0x0103]
                `shouldBe` [0x00, 0xC3, 0x50, 0x01]

        it "reads the cartridge type byte at 0x0147" $ do
            let Right c = loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            read8 0x0147 c `shouldBe` 0x00

        it "returns 0xFF for ERAM reads when the cartridge has no RAM" $ do
            let Right c = loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
            read8 0xA000 c `shouldBe` 0xFF

    describe "write8 (NoMbc)" $ do
        it "ignores writes to the ROM window" $ do
            let Right c0 = loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
                c1 = write8 0x0100 0xAB c0
            read8 0x0100 c1 `shouldBe` 0x00

        it "round-trips RAM writes when the cartridge has 8 KiB ERAM" $ do
            let Right c0 = loadRom (mkSyntheticRom 0x08 0x00 0x02 "X")
                c1 = write8 0xA000 0xAB (write8 0xBFFF 0xCD c0)
            read8 0xA000 c1 `shouldBe` 0xAB
            read8 0xBFFF c1 `shouldBe` 0xCD

        it "drops RAM writes when the cartridge has no RAM" $ do
            let Right c0 = loadRom (mkSyntheticRom 0x00 0x00 0x00 "X")
                c1 = write8 0xA000 0xAB c0
            read8 0xA000 c1 `shouldBe` 0xFF

    describe "external/gb-test-roms" $ do
        it "parses cpu_instrs.gb header as MBC1, 64 KiB" $ do
            mb <- tryReadFile "external/gb-test-roms/cpu_instrs/cpu_instrs.gb"
            case mb of
                Nothing -> pendingWith "external/gb-test-roms submodule not initialized"
                Just bytes -> do
                    case loadRom bytes of
                        Left (UnsupportedMbcKind Mbc1) -> pure ()
                        Left other -> expectationFailure ("expected UnsupportedMbcKind Mbc1, got: " ++ show other)
                        Right _ -> expectationFailure "expected UnsupportedMbcKind, got Right"
                    case parseHeaderFromBytes bytes of
                        Right h -> do
                            hdrMbcKind h `shouldBe` Mbc1
                            hdrRomBytes h `shouldBe` 64 * 1024
                        Left e -> expectationFailure (show e)

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile path = do
    r <- try (BS.readFile path) :: IO (Either IOException ByteString)
    pure (either (const Nothing) Just r)

parseHeaderFromBytes :: ByteString -> Either HeaderError Header
parseHeaderFromBytes = parseHeader

mkSyntheticRom :: Word8 -> Word8 -> Word8 -> String -> ByteString
mkSyntheticRom cartType romCode ramCode title =
    let romSize = 32 * 1024 `shiftL` fromIntegral romCode
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
