{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cartridge.HeaderSpec (spec) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge.Header
import Test.Hspec

spec :: Spec
spec = do
    describe "parseHeader" $ do
        it "rejects ROMs shorter than 0x150 bytes" $
            parseHeader (BS.replicate 0x100 0) `shouldBe` Left (RomTooShort 0x100)

        it "parses a synthetic NoMbc 32 KiB ROM" $ do
            let rom = mkSyntheticRom 0x00 0x00 0x00 "HELLO"
            case parseHeader rom of
                Right h -> do
                    hdrTitle h `shouldBe` "HELLO"
                    hdrMbcKind h `shouldBe` NoMbc
                    capRam (hdrCaps h) `shouldBe` False
                    hdrRomBytes h `shouldBe` 32 * 1024
                    hdrRamBytes h `shouldBe` 0
                    hdrCgbFlag h `shouldBe` DmgOnly
                    hdrSgbFlag h `shouldBe` False
                Left e -> expectationFailure (show e)

        it "decodes MBC1+RAM+BATTERY (cart type 0x03) with 8 KiB RAM" $ do
            let rom = mkSyntheticRom 0x03 0x00 0x02 "GAME"
            case parseHeader rom of
                Right h -> do
                    hdrMbcKind h `shouldBe` Mbc1
                    capRam (hdrCaps h) `shouldBe` True
                    capBattery (hdrCaps h) `shouldBe` True
                    capTimer (hdrCaps h) `shouldBe` False
                    hdrRamBytes h `shouldBe` 8 * 1024
                Left e -> expectationFailure (show e)

        it "decodes MBC5+RUMBLE+RAM+BATTERY (cart type 0x1E) with 128 KiB RAM" $ do
            let rom = mkSyntheticRom 0x1E 0x00 0x04 "WL3"
            case parseHeader rom of
                Right h -> do
                    hdrMbcKind h `shouldBe` Mbc5
                    capRumble (hdrCaps h) `shouldBe` True
                    capRam (hdrCaps h) `shouldBe` True
                    capBattery (hdrCaps h) `shouldBe` True
                    hdrRamBytes h `shouldBe` 128 * 1024
                Left e -> expectationFailure (show e)

        it "rejects ROMs with a corrupted header checksum" $ do
            let good = mkSyntheticRom 0x00 0x00 0x00 "X"
                bad = patchByte 0x014D (BS.index good 0x014D + 1) good
            case parseHeader bad of
                Left (BadHeaderChecksum _ _) -> pure ()
                other -> expectationFailure ("expected BadHeaderChecksum, got: " ++ show other)

        it "rejects ROMs with an invalid ROM size code" $ do
            let bad = patchByte 0x0148 0xFF (mkSyntheticRom 0x00 0x00 0x00 "X")
            case parseHeader bad of
                Left (InvalidRomSizeCode 0xFF) -> pure ()
                other -> expectationFailure ("expected InvalidRomSizeCode, got: " ++ show other)

        it "rejects ROMs with an invalid RAM size code" $ do
            let bad = patchByte 0x0149 0x09 (mkSyntheticRom 0x00 0x00 0x00 "X")
            case parseHeader bad of
                Left (InvalidRamSizeCode 0x09) -> pure ()
                other -> expectationFailure ("expected InvalidRamSizeCode, got: " ++ show other)

        it "decodes CGB-only flag at 0x0143" $ do
            let rom = patchByte 0x0143 0xC0 (mkSyntheticRom 0x00 0x00 0x00 "X")
                fixed = fixupChecksum rom
            case parseHeader fixed of
                Right h -> hdrCgbFlag h `shouldBe` CgbOnly
                Left e -> expectationFailure (show e)

    {- The cartridge type byte is what picks the MBC implementation, so a wrong row
    here silently runs a game on the wrong bank controller. The whole documented table
    is listed rather than sampled, because the rows are independent data and a typo in
    one says nothing about its neighbours. -}
    describe "cartridge type table" $ do
        let caps = Capabilities
            expected =
                [ (0x00, NoMbc, caps False False False False)
                , (0x08, NoMbc, caps True False False False)
                , (0x09, NoMbc, caps True True False False)
                , (0x01, Mbc1, caps False False False False)
                , (0x02, Mbc1, caps True False False False)
                , (0x03, Mbc1, caps True True False False)
                , (0x05, Mbc2, caps False False False False)
                , (0x06, Mbc2, caps False True False False)
                , (0x0F, Mbc3, caps False True True False)
                , (0x10, Mbc3, caps True True True False)
                , (0x11, Mbc3, caps False False False False)
                , (0x12, Mbc3, caps True False False False)
                , (0x13, Mbc3, caps True True False False)
                , (0x19, Mbc5, caps False False False False)
                , (0x1A, Mbc5, caps True False False False)
                , (0x1B, Mbc5, caps True True False False)
                , (0x1C, Mbc5, caps False False False True)
                , (0x1D, Mbc5, caps True False False True)
                , (0x1E, Mbc5, caps True True False True)
                , (0xFF, HuC1, caps True True False False)
                ]

        mapM_
            ( \(byte, kind, cap) ->
                it ("decodes cartridge type " ++ show byte ++ " as " ++ show kind) $ do
                    -- RAM code 0x02 keeps every row parseable, including the ones that
                    -- declare no RAM; the capability bits come from the type byte alone.
                    let rom = mkSyntheticRom byte 0x00 0x02 "TYPE"
                    case parseHeader rom of
                        Right h -> (hdrMbcKind h, hdrCaps h) `shouldBe` (kind, cap)
                        Left e -> expectationFailure (show e)
            )
            expected

        it "reports an unrecognised type byte as UnknownMbc with no capabilities" $ do
            -- 0x22 is MBC7 on real hardware, which Ocelot does not implement. Surfacing
            -- the raw byte is what lets a frontend say which cart it could not run.
            let rom = mkSyntheticRom 0x22 0x00 0x02 "MBC7"
            case parseHeader rom of
                Right h -> do
                    hdrMbcKind h `shouldBe` UnknownMbc 0x22
                    hdrCaps h `shouldBe` Capabilities False False False False
                Left e -> expectationFailure (show e)

    describe "RAM size table" $ do
        let sizes =
                [ (0x00, 0)
                , (0x01, 2 * 1024)
                , (0x02, 8 * 1024)
                , (0x03, 32 * 1024)
                , (0x04, 128 * 1024)
                , (0x05, 64 * 1024)
                ]
        mapM_
            ( \(code, bytes) ->
                it ("decodes RAM size code " ++ show code ++ " as " ++ show bytes ++ " bytes") $ do
                    let rom = mkSyntheticRom 0x03 0x00 code "RAM"
                    case parseHeader rom of
                        Right h -> hdrRamBytes h `shouldBe` bytes
                        Left e -> expectationFailure (show e)
            )
            sizes

{- | Build a 32 KiB ROM with the requested header fields. The header checksum byte at 0x014D is
computed from the surrounding bytes so the resulting ROM always parses cleanly. ROM size code is
restricted to 0x00 (32 KiB) to keep the helper small; tests that need larger ROMs can patch and
refixup.
-}
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
            , (0x0146, 0x00) -- SGB
            , (0x0147, cartType)
            , (0x0148, romCode)
            , (0x0149, ramCode)
            , (0x014A, 0x00) -- Destination
            , (0x014B, 0x33) -- Old licensee = "use new licensee"
            , (0x014C, 0x00) -- ROM version
            ]
                <> titleBytes
        v1 = v0 V.// fields
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.pack (V.toList (v1 V.// [(0x014D, cs)]))

patchByte :: Int -> Word8 -> ByteString -> ByteString
patchByte i v bs =
    BS.take i bs `BS.append` BS.singleton v `BS.append` BS.drop (i + 1) bs

{- | Recompute the header checksum byte for a ROM after manual edits to bytes in the
 @0x0134..0x014C@ range.
-}
fixupChecksum :: ByteString -> ByteString
fixupChecksum bs = patchByte 0x014D (expectedHeaderChecksum bs) bs
