{- | Parse the cartridge header at @0x0100..0x014F@.

The header layout is documented in pandocs (<https://gbdev.io/pandocs/The_Cartridge_Header.html>).
Only fields that influence emulator behavior are decoded here; the Nintendo logo bytes are not
validated, since the boot ROM check that uses them is not yet implemented.
-}
module Ocelot.Cartridge.Header (
    Header (..),
    CgbFlag (..),
    Destination (..),
    MbcKind (..),
    Capabilities (..),
    HeaderError (..),
    headerEnd,
    parseHeader,
    expectedHeaderChecksum,
) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, isPrint)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word8)

{- | The header occupies bytes @0x0100..0x014F@; @headerEnd@ is the smallest
ROM size that contains a complete header.
-}
headerEnd :: Int
headerEnd = 0x0150

data Header = Header
    { hdrTitle :: !Text
    , hdrCgbFlag :: !CgbFlag
    , hdrSgbFlag :: !Bool
    , hdrMbcKind :: !MbcKind
    , hdrCaps :: !Capabilities
    , hdrRomBytes :: !Int
    , hdrRamBytes :: !Int
    , hdrDestination :: !Destination
    , hdrVersion :: !Word8
    , hdrHeaderChecksum :: !Word8
    , hdrGlobalChecksum :: !Word16
    }
    deriving (Eq, Show)

data CgbFlag = DmgOnly | DmgAndCgb | CgbOnly
    deriving (Eq, Show)

data Destination = Japan | International
    deriving (Eq, Show)

{- | Memory-bank controller family. Variant-specific capabilities (RAM, battery,
timer, rumble) are kept separate in 'Capabilities' so the same MBC family can
describe several cartridge type bytes.
-}
data MbcKind
    = NoMbc
    | Mbc1
    | Mbc2
    | Mbc3
    | Mbc5
    | HuC1
    | UnknownMbc !Word8
    deriving (Eq, Show)

data Capabilities = Capabilities
    { capRam :: !Bool
    , capBattery :: !Bool
    , capTimer :: !Bool
    , capRumble :: !Bool
    }
    deriving (Eq, Show)

data HeaderError
    = RomTooShort !Int
    | -- | First argument is the expected checksum, second is the value stored at @0x014D@.
      BadHeaderChecksum !Word8 !Word8
    | InvalidRomSizeCode !Word8
    | InvalidRamSizeCode !Word8
    deriving (Eq, Show)

parseHeader :: ByteString -> Either HeaderError Header
parseHeader rom
    | BS.length rom < headerEnd = Left (RomTooShort (BS.length rom))
    | otherwise = do
        let at = BS.index rom
            (mbcKind, caps) = decodeCartridgeType (at 0x0147)
            cgb = decodeCgbFlag (at 0x0143)
            sgb = at 0x0146 == 0x03
            stored = at 0x014D
            expected = expectedHeaderChecksum rom
            global =
                (fromIntegral (at 0x014E) `shiftL` 8)
                    + fromIntegral (at 0x014F)
            dest = if at 0x014A == 0 then Japan else International
        romBytes <- decodeRomSize (at 0x0148)
        ramBytes <- decodeRamSize (at 0x0149)
        if stored /= expected
            then Left (BadHeaderChecksum expected stored)
            else
                Right
                    Header
                        { hdrTitle = decodeTitle rom
                        , hdrCgbFlag = cgb
                        , hdrSgbFlag = sgb
                        , hdrMbcKind = mbcKind
                        , hdrCaps = caps
                        , hdrRomBytes = romBytes
                        , hdrRamBytes = ramBytes
                        , hdrDestination = dest
                        , hdrVersion = at 0x014C
                        , hdrHeaderChecksum = stored
                        , hdrGlobalChecksum = global
                        }

{- | The header checksum is the byte that satisfies, for @x@ initialized to 0:
@for i in 0x0134..=0x014C: x := (x - rom[i] - 1) .&. 0xFF@.
-}
expectedHeaderChecksum :: ByteString -> Word8
expectedHeaderChecksum rom = foldl' step 0 [0x0134 .. 0x014C]
  where
    step acc i = acc - BS.index rom i - 1

decodeCgbFlag :: Word8 -> CgbFlag
decodeCgbFlag w
    | w == 0x80 = DmgAndCgb
    | w == 0xC0 = CgbOnly
    | otherwise = DmgOnly

decodeCartridgeType :: Word8 -> (MbcKind, Capabilities)
decodeCartridgeType w = case w of
    0x00 -> (NoMbc, noCaps)
    0x08 -> (NoMbc, noCaps{capRam = True})
    0x09 -> (NoMbc, noCaps{capRam = True, capBattery = True})
    0x01 -> (Mbc1, noCaps)
    0x02 -> (Mbc1, noCaps{capRam = True})
    0x03 -> (Mbc1, noCaps{capRam = True, capBattery = True})
    0x05 -> (Mbc2, noCaps)
    0x06 -> (Mbc2, noCaps{capBattery = True})
    0x0F -> (Mbc3, noCaps{capTimer = True, capBattery = True})
    0x10 -> (Mbc3, noCaps{capTimer = True, capRam = True, capBattery = True})
    0x11 -> (Mbc3, noCaps)
    0x12 -> (Mbc3, noCaps{capRam = True})
    0x13 -> (Mbc3, noCaps{capRam = True, capBattery = True})
    0x19 -> (Mbc5, noCaps)
    0x1A -> (Mbc5, noCaps{capRam = True})
    0x1B -> (Mbc5, noCaps{capRam = True, capBattery = True})
    0x1C -> (Mbc5, noCaps{capRumble = True})
    0x1D -> (Mbc5, noCaps{capRumble = True, capRam = True})
    0x1E -> (Mbc5, noCaps{capRumble = True, capRam = True, capBattery = True})
    0xFF -> (HuC1, noCaps{capRam = True, capBattery = True})
    other -> (UnknownMbc other, noCaps)
  where
    noCaps = Capabilities False False False False

decodeRomSize :: Word8 -> Either HeaderError Int
decodeRomSize w
    | w <= 0x08 = Right (32 * 1024 `shiftL` fromIntegral w)
    | otherwise = Left (InvalidRomSizeCode w)

decodeRamSize :: Word8 -> Either HeaderError Int
decodeRamSize w = case w of
    0x00 -> Right 0
    0x01 -> Right (2 * 1024)
    0x02 -> Right (8 * 1024)
    0x03 -> Right (32 * 1024)
    0x04 -> Right (128 * 1024)
    0x05 -> Right (64 * 1024)
    other -> Left (InvalidRamSizeCode other)

decodeTitle :: ByteString -> Text
decodeTitle rom =
    let raw = BS.take 16 (BS.drop 0x0134 rom)
        kept = BS.takeWhile printable raw
        printable b = b /= 0 && isPrint (chr (fromIntegral b))
     in T.pack (map (chr . fromIntegral) (BS.unpack kept))
