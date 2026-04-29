{- | Cartridge facade.

The bus calls 'read8' and 'write8' for the @0x0000-0x7FFF@ ROM window and the
@0xA000-0xBFFF@ external RAM window. MBC variant selection, header parsing,
and (eventually) battery-backed save handling stay inside this module; outside
callers see 'Cartridge' as an opaque type.

Currently only no-MBC cartridges are implemented. Other MBC kinds parse but
'loadRom' returns 'UnsupportedMbcKind'.
-}
module Ocelot.Cartridge (
    Cartridge,
    CartridgeError (..),
    cartridgeHeader,
    loadRom,
    read8,
    write8,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)
import Ocelot.Cartridge.Header (
    Header (..),
    HeaderError,
    MbcKind (..),
    parseHeader,
 )

data Cartridge = Cartridge
    { cartHeader :: !Header
    , cartRom :: !ByteString
    , cartRam :: !(Vector Word8)
    , cartImpl :: !MbcImpl
    }

instance Show Cartridge where
    show c =
        "Cartridge { header = "
            ++ show (cartHeader c)
            ++ ", romBytes = "
            ++ show (BS.length (cartRom c))
            ++ ", ramBytes = "
            ++ show (V.length (cartRam c))
            ++ " }"

instance Eq Cartridge where
    a == b =
        cartHeader a == cartHeader b
            && cartRom a == cartRom b
            && cartRam a == cartRam b

data MbcImpl = NoMbcImpl
    deriving (Eq, Show)

data CartridgeError
    = HeaderParse HeaderError
    | UnsupportedMbcKind MbcKind
    deriving (Eq, Show)

cartridgeHeader :: Cartridge -> Header
cartridgeHeader = cartHeader

loadRom :: ByteString -> Either CartridgeError Cartridge
loadRom raw = do
    hdr <- first HeaderParse (parseHeader raw)
    impl <- selectImpl (hdrMbcKind hdr)
    let ram = V.replicate (hdrRamBytes hdr) 0xFF
    pure
        Cartridge
            { cartHeader = hdr
            , cartRom = raw
            , cartRam = ram
            , cartImpl = impl
            }

selectImpl :: MbcKind -> Either CartridgeError MbcImpl
selectImpl NoMbc = Right NoMbcImpl
selectImpl other = Left (UnsupportedMbcKind other)

{- | Bus-side read. Defined for the cartridge windows @0x0000-0x7FFF@ (ROM) and
@0xA000-0xBFFF@ (external RAM); calling with other addresses is a programmer
error and returns @0xFF@ as a safe fallback.
-}
read8 :: Word16 -> Cartridge -> Word8
read8 addr c = case cartImpl c of
    NoMbcImpl -> noMbcRead addr c

{- | Bus-side write. ROM-window writes to a no-MBC cartridge are ignored; RAM
writes update the external RAM in place.
-}
write8 :: Word16 -> Word8 -> Cartridge -> Cartridge
write8 addr v c = case cartImpl c of
    NoMbcImpl -> noMbcWrite addr v c

noMbcRead :: Word16 -> Cartridge -> Word8
noMbcRead addr c
    | addr <= 0x7FFF =
        let i = fromIntegral addr
         in if i < BS.length (cartRom c) then BS.index (cartRom c) i else 0xFF
    | addr >= 0xA000 && addr <= 0xBFFF =
        let i = fromIntegral (addr - 0xA000)
         in if i < V.length (cartRam c) then cartRam c V.! i else 0xFF
    | otherwise = 0xFF

noMbcWrite :: Word16 -> Word8 -> Cartridge -> Cartridge
noMbcWrite addr v c
    | addr >= 0xA000 && addr <= 0xBFFF =
        let i = fromIntegral (addr - 0xA000)
         in if i < V.length (cartRam c)
                then c{cartRam = cartRam c V.// [(i, v)]}
                else c
    | otherwise = c
