{-# LANGUAGE BangPatterns #-}

{- | Cartridge facade.

The bus calls 'read8' and 'write8' for the @0x0000-0x7FFF@ ROM window and the
@0xA000-0xBFFF@ external RAM window. MBC variant selection, header parsing,
and (eventually) battery-backed save handling stay inside this module; outside
callers see 'Cartridge' as an opaque type.

Internally the cartridge holds its MBC state in an 'IORef' and its RAM in a
mutable 'IOVector', so reads and writes on a hot ROM are O(1) instead of
O(N). This is the reason the public API lives in @IO@.

Supported MBC kinds: NoMbc, MBC1, MBC2 (with built-in 512-nibble RAM),
MBC3 (with RTC), and MBC5. Other kinds parse but 'loadRom' returns
'UnsupportedMbcKind'. The MBC3 RTC tracks host wall-clock time (POSIX
seconds) and is persisted across emulator restarts via
'extractSave' / 'loadSave', which append a 48-byte VBA-M-compatible
suffix to the RAM bytes.
-}
module Ocelot.Cartridge (
    Cartridge,
    CartridgeError (..),
    cartridgeHeader,
    cartridgeHasBattery,
    extractRam,
    loadRam,
    extractSave,
    loadSave,
    dumpMbc,
    loadMbc,
    loadRom,
    read8,
    write8,
) where

import Control.Monad (forM_, when)
import Data.Bifunctor (first)
import Data.Bits (shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word32, Word8)
import Ocelot.Cartridge.Header (
    Capabilities (..),
    Header (..),
    HeaderError,
    MbcKind (..),
    parseHeader,
 )

data Cartridge = Cartridge
    { cartHeader :: !Header
    , cartRom :: !ByteString
    , cartRam :: !(MV.IOVector Word8)
    , cartImpl :: !(IORef MbcImpl)
    }

data MbcImpl
    = NoMbcImpl
    | Mbc1Impl !Mbc1State
    | Mbc2Impl !Mbc2State
    | Mbc3Impl !Mbc3State
    | Mbc5Impl !Mbc5State
    | HuC1Impl !HuC1State
    deriving (Eq, Show)

data Mbc1State = Mbc1State
    { m1RamEnabled :: !Bool
    , m1RomBankLow :: !Word8
    , m1BankHi :: !Word8
    , m1Mode :: !Bool
    , m1Multicart :: !Bool
    -- ^ True for the MBC1M wiring variant: bank-low is only 4 bits
    -- wide (not 5) and the bank-hi register shifts by 4 instead of 5,
    -- so the cart can hold 4 sub-games of 16 banks each in 1 MiB.
    -- Detected at 'loadRom' by matching the Nintendo logo at the
    -- start of the cart against a copy at offset @0x40000@. Mooneye
    -- 'emulator-only/mbc1/multicart_rom_8Mb' verifies this.
    }
    deriving (Eq, Show)

initialMbc1 :: Mbc1State
initialMbc1 =
    Mbc1State
        { m1RamEnabled = False
        , m1RomBankLow = 0x01
        , m1BankHi = 0x00
        , m1Mode = False
        , m1Multicart = False
        }

data Mbc2State = Mbc2State
    { m2RamEnabled :: !Bool
    , m2RomBank :: !Word8
    -- ^ 4-bit ROM bank number; 0 wraps to 1.
    }
    deriving (Eq, Show)

initialMbc2 :: Mbc2State
initialMbc2 =
    Mbc2State
        { m2RamEnabled = False
        , m2RomBank = 0x01
        }

data Mbc3State = Mbc3State
    { m3RamRtcEnabled :: !Bool
    , m3RomBank :: !Word8
    , m3RamBankOrRtc :: !Word8
    , m3LatchPrev :: !Word8
    -- ^ Last byte written to @0x6000-0x7FFF@. A @0x00@ followed by @0x01@
    -- latches the live RTC into 'm3RtcLatched'.
    , m3RtcSecBase :: !Integer
    -- ^ Elapsed-second count at the anchor instant. When halted this is
    -- the frozen value; when running, the live count is
    -- @m3RtcSecBase + (now - m3RtcAnchor)@.
    , m3RtcAnchor :: !Integer
    -- ^ POSIX seconds at which 'm3RtcSecBase' was set. Only meaningful
    -- when not halted.
    , m3RtcHalted :: !Bool
    , m3RtcDayCarry :: !Bool
    -- ^ Sticky DH bit 7. Cleared only by writing 0 to DH bit 7.
    , m3RtcLatched :: !RtcRegs
    }
    deriving (Eq, Show)

{- | The five RTC registers as exposed at @0xA000-0xBFFF@ when
'm3RamBankOrRtc' selects bank @0x08..0x0C@.
-}
data RtcRegs = RtcRegs
    { rrS :: !Word8
    , rrM :: !Word8
    , rrH :: !Word8
    , rrDL :: !Word8
    , rrDH :: !Word8
    }
    deriving (Eq, Show)

zeroRtcRegs :: RtcRegs
zeroRtcRegs = RtcRegs 0 0 0 0 0

initialMbc3 :: Mbc3State
initialMbc3 =
    Mbc3State
        { m3RamRtcEnabled = False
        , m3RomBank = 0x01
        , m3RamBankOrRtc = 0x00
        , m3LatchPrev = 0xFF
        , m3RtcSecBase = 0
        , m3RtcAnchor = 0
        , m3RtcHalted = False
        , m3RtcDayCarry = False
        , m3RtcLatched = zeroRtcRegs
        }

data HuC1State = HuC1State
    { hcRamEnabled :: !Bool
    -- ^ Approximation: true when the most recent write to 0x0000-0x1FFF
    -- selected RAM mode (low nibble != 0xE). False when IR mode was
    -- selected; we ignore IR entirely and just gate RAM access.
    , hcRomBank :: !Word8
    -- ^ 6-bit ROM bank selector at 0x2000-0x3FFF. Unlike MBC1, zero is
    -- not auto-translated to one.
    , hcRamBank :: !Word8
    -- ^ 2-bit RAM bank selector at 0x4000-0x5FFF.
    }
    deriving (Eq, Show)

initialHuC1 :: HuC1State
initialHuC1 = HuC1State{hcRamEnabled = False, hcRomBank = 0x00, hcRamBank = 0x00}

data Mbc5State = Mbc5State
    { m5RamEnabled :: !Bool
    , m5RomBankLow :: !Word8
    , m5RomBankHigh :: !Word8
    , m5RamBank :: !Word8
    }
    deriving (Eq, Show)

initialMbc5 :: Mbc5State
initialMbc5 =
    Mbc5State
        { m5RamEnabled = False
        , m5RomBankLow = 0x01
        , m5RomBankHigh = 0x00
        , m5RamBank = 0x00
        }

data CartridgeError
    = HeaderParse HeaderError
    | UnsupportedMbcKind MbcKind
    deriving (Eq, Show)

cartridgeHeader :: Cartridge -> Header
cartridgeHeader = cartHeader

{- | Whether the cartridge has battery-backed state worth writing to a
@.sav@ file: external RAM, an RTC, or MBC2's built-in 512-nibble RAM.
Returns 'False' for cartridges with no battery flag at all.
-}
cartridgeHasBattery :: Cartridge -> Bool
cartridgeHasBattery c =
    let caps = hdrCaps (cartHeader c)
        mbc = hdrMbcKind (cartHeader c)
     in capBattery caps && (capRam caps || capTimer caps || mbc == Mbc2)

{- | Take a snapshot of the cartridge's external RAM as an immutable byte
string. Used by the frontend to write @.sav@ files on exit.
-}
extractRam :: Cartridge -> IO ByteString
extractRam c = do
    frozen <- V.freeze (cartRam c)
    pure (BS.pack (V.toList frozen))

{- | Overwrite the cartridge's external RAM with the given bytes. Bytes
beyond the cartridge's RAM size are discarded; if fewer bytes are supplied
than the cart's RAM, the remainder is left at its prior value. Used by the
frontend to load @.sav@ files at startup.
-}
loadRam :: ByteString -> Cartridge -> IO ()
loadRam bs c = do
    let n = min (BS.length bs) (MV.length (cartRam c))
    forM_ [0 .. n - 1] $ \i ->
        MV.write (cartRam c) i (BS.index bs i)

{- | Serialize the full battery-backed state for a @.sav@ file: the
cartridge's external RAM, optionally followed by the 48-byte RTC suffix
when the cart has an MBC3 RTC. The RTC suffix layout is the
VBA-M-compatible one (10 little-endian @uint32@s for live then latched
S\/M\/H\/DL\/DH, then a little-endian @int64@ POSIX timestamp).
-}
extractSave :: Cartridge -> IO ByteString
extractSave c = do
    ram <- extractRam c
    rtc <- extractRtcSuffix c
    pure (ram <> rtc)

{- | Deserialize a @.sav@ file, applying the RAM portion and any
trailing RTC suffix. Older RAM-only @.sav@ files are accepted: the RTC
suffix is parsed only when the file is at least 48 bytes longer than
the cart's RAM and the cart has an MBC3 RTC.
-}
loadSave :: ByteString -> Cartridge -> IO ()
loadSave bs c = do
    let ramSize = MV.length (cartRam c)
        (ramBytes, rest) = BS.splitAt ramSize bs
    loadRam ramBytes c
    when (BS.length rest >= rtcSuffixSize) (applyRtcSuffix rest c)

{- | Snapshot the live MBC bank-select state (not the RAM contents; use
'extractSave' for those). The blob shape is fixed per MBC kind; an MBC
mismatch on 'loadMbc' is silently ignored (the bank state stays as-is).
-}
dumpMbc :: Cartridge -> IO ByteString
dumpMbc c = do
    impl <- readIORef (cartImpl c)
    pure (BL.toStrict (BB.toLazyByteString (encodeMbc impl)))

loadMbc :: ByteString -> Cartridge -> IO ()
loadMbc bs c = do
    cur <- readIORef (cartImpl c)
    for_ (decodeMbc cur bs) (writeIORef (cartImpl c))

encodeMbc :: MbcImpl -> BB.Builder
encodeMbc NoMbcImpl = BB.word8 0x00
encodeMbc (Mbc1Impl s) =
    BB.word8 0x01
        <> BB.word8 (if m1RamEnabled s then 1 else 0)
        <> BB.word8 (m1RomBankLow s)
        <> BB.word8 (m1BankHi s)
        <> BB.word8 (if m1Mode s then 1 else 0)
encodeMbc (Mbc2Impl s) =
    BB.word8 0x02
        <> BB.word8 (if m2RamEnabled s then 1 else 0)
        <> BB.word8 (m2RomBank s)
encodeMbc (Mbc3Impl s) =
    BB.word8 0x03
        <> BB.word8 (if m3RamRtcEnabled s then 1 else 0)
        <> BB.word8 (m3RomBank s)
        <> BB.word8 (m3RamBankOrRtc s)
        <> BB.word8 (m3LatchPrev s)
        <> BB.int64LE (fromIntegral (m3RtcSecBase s))
        <> BB.int64LE (fromIntegral (m3RtcAnchor s))
        <> BB.word8 (if m3RtcHalted s then 1 else 0)
        <> BB.word8 (if m3RtcDayCarry s then 1 else 0)
        <> encodeRtcRegs (m3RtcLatched s)
encodeMbc (Mbc5Impl s) =
    BB.word8 0x05
        <> BB.word8 (if m5RamEnabled s then 1 else 0)
        <> BB.word8 (m5RomBankLow s)
        <> BB.word8 (m5RomBankHigh s)
        <> BB.word8 (m5RamBank s)
encodeMbc (HuC1Impl s) =
    BB.word8 0xFF
        <> BB.word8 (if hcRamEnabled s then 1 else 0)
        <> BB.word8 (hcRomBank s)
        <> BB.word8 (hcRamBank s)

decodeMbc :: MbcImpl -> ByteString -> Maybe MbcImpl
decodeMbc cur bs
    | BS.null bs = Nothing
    | otherwise = case (BS.head bs, cur) of
        (0x00, NoMbcImpl) -> Just NoMbcImpl
        (0x01, Mbc1Impl curS) ->
            let p = BS.drop 1 bs
             in if BS.length p < 4
                    then Nothing
                    else
                        Just $
                            Mbc1Impl
                                Mbc1State
                                    { m1RamEnabled = BS.index p 0 /= 0
                                    , m1RomBankLow = BS.index p 1
                                    , m1BankHi = BS.index p 2
                                    , m1Mode = BS.index p 3 /= 0
                                    , -- Multicart wiring is detected at
                                      -- 'loadRom', not snapshotted.
                                      m1Multicart = m1Multicart curS
                                    }
        (0x02, Mbc2Impl _) ->
            let p = BS.drop 1 bs
             in if BS.length p < 2
                    then Nothing
                    else
                        Just $
                            Mbc2Impl
                                Mbc2State
                                    { m2RamEnabled = BS.index p 0 /= 0
                                    , m2RomBank = BS.index p 1
                                    }
        (0x03, Mbc3Impl _) ->
            let p = BS.drop 1 bs
             in if BS.length p < 4 + 16 + 2 + 20
                    then Nothing
                    else
                        let secBase = fromIntegral (decodeI64LE (BS.drop 4 p))
                            anchor = fromIntegral (decodeI64LE (BS.drop 12 p))
                            halted = BS.index p 20 /= 0
                            carry = BS.index p 21 /= 0
                            latched = decodeRtcRegs (BS.drop 22 p)
                         in Just $
                                Mbc3Impl
                                    Mbc3State
                                        { m3RamRtcEnabled = BS.index p 0 /= 0
                                        , m3RomBank = BS.index p 1
                                        , m3RamBankOrRtc = BS.index p 2
                                        , m3LatchPrev = BS.index p 3
                                        , m3RtcSecBase = secBase
                                        , m3RtcAnchor = anchor
                                        , m3RtcHalted = halted
                                        , m3RtcDayCarry = carry
                                        , m3RtcLatched = latched
                                        }
        (0x05, Mbc5Impl _) ->
            let p = BS.drop 1 bs
             in if BS.length p < 4
                    then Nothing
                    else
                        Just $
                            Mbc5Impl
                                Mbc5State
                                    { m5RamEnabled = BS.index p 0 /= 0
                                    , m5RomBankLow = BS.index p 1
                                    , m5RomBankHigh = BS.index p 2
                                    , m5RamBank = BS.index p 3
                                    }
        (0xFF, HuC1Impl _) ->
            let p = BS.drop 1 bs
             in if BS.length p < 3
                    then Nothing
                    else
                        Just $
                            HuC1Impl
                                HuC1State
                                    { hcRamEnabled = BS.index p 0 /= 0
                                    , hcRomBank = BS.index p 1
                                    , hcRamBank = BS.index p 2
                                    }
        _ -> Nothing

loadRom :: ByteString -> IO (Either CartridgeError Cartridge)
loadRom raw =
    case do
        hdr <- first HeaderParse (parseHeader raw)
        impl <- selectInitialImpl (hdrMbcKind hdr)
        pure (hdr, impl) of
        Left err -> pure (Left err)
        Right (hdr, impl0) -> do
            -- MBC2 has 512 nibbles of built-in RAM; the header reports zero
            -- RAM size for those carts, so override the allocation here.
            let ramBytes = case hdrMbcKind hdr of
                    Mbc2 -> 512
                    _ -> hdrRamBytes hdr
            ram <- MV.replicate ramBytes 0xFF
            -- MBC1 multicart detection: a cart big enough to hold a
            -- Nintendo-logo copy at offset 0x40000 with the same bytes
            -- as the canonical logo at 0x104 is the MBC1M wiring (4
            -- sub-games of 16 banks each in 1 MiB). Matches SameBoy
            -- 'mbc.c' line 254.
            let impl1 = case impl0 of
                    Mbc1Impl s
                        | detectMbc1Multicart raw ->
                            Mbc1Impl s{m1Multicart = True}
                    other -> other
            impl <- anchorRtc impl1
            ref <- newIORef impl
            pure $
                Right
                    Cartridge
                        { cartHeader = hdr
                        , cartRom = raw
                        , cartRam = ram
                        , cartImpl = ref
                        }

{- | Heuristic for MBC1M (multicart) wiring: the cart is at least
@0x44000@ bytes long and the Nintendo logo at @0x104..0x133@ is
duplicated at @0x40104..0x40133@. Real-hardware MBC1 carts that ship
4 sub-games of 16 banks each carry a copy of the logo at the start
of each sub-cart. SameBoy uses the same heuristic.
-}
detectMbc1Multicart :: ByteString -> Bool
detectMbc1Multicart raw =
    BS.length raw >= 0x44000
        && BS.take 0x30 (BS.drop 0x104 raw)
            == BS.take 0x30 (BS.drop 0x40104 raw)

{- | Set the MBC3 RTC anchor to "now" so the live count starts at zero.
Cartridges with no RTC pass through unchanged.
-}
anchorRtc :: MbcImpl -> IO MbcImpl
anchorRtc (Mbc3Impl s) = do
    now <- nowPosix
    pure (Mbc3Impl s{m3RtcAnchor = now})
anchorRtc other = pure other

-- | Current POSIX time as an integer second count.
nowPosix :: IO Integer
nowPosix = floor <$> getPOSIXTime

selectInitialImpl :: MbcKind -> Either CartridgeError MbcImpl
selectInitialImpl NoMbc = Right NoMbcImpl
selectInitialImpl Mbc1 = Right (Mbc1Impl initialMbc1)
selectInitialImpl Mbc2 = Right (Mbc2Impl initialMbc2)
selectInitialImpl Mbc3 = Right (Mbc3Impl initialMbc3)
selectInitialImpl Mbc5 = Right (Mbc5Impl initialMbc5)
selectInitialImpl HuC1 = Right (HuC1Impl initialHuC1)
selectInitialImpl other = Left (UnsupportedMbcKind other)

read8 :: Word16 -> Cartridge -> IO Word8
read8 addr c = do
    impl <- readIORef (cartImpl c)
    case impl of
        NoMbcImpl -> noMbcRead addr c
        Mbc1Impl s -> mbc1Read s addr c
        Mbc2Impl s -> mbc2Read s addr c
        Mbc3Impl s -> mbc3Read s addr c
        Mbc5Impl s -> mbc5Read s addr c
        HuC1Impl s -> huc1Read s addr c

write8 :: Word16 -> Word8 -> Cartridge -> IO ()
write8 addr v c = do
    impl <- readIORef (cartImpl c)
    case impl of
        NoMbcImpl -> noMbcWrite addr v c
        Mbc1Impl s -> mbc1Write s addr v c
        Mbc2Impl s -> mbc2Write s addr v c
        Mbc3Impl s -> mbc3Write s addr v c
        Mbc5Impl s -> mbc5Write s addr v c
        HuC1Impl s -> huc1Write s addr v c

----------------------------------------------------------------------
-- NoMbc
----------------------------------------------------------------------

noMbcRead :: Word16 -> Cartridge -> IO Word8
noMbcRead addr c
    | addr <= 0x7FFF =
        let i = fromIntegral addr
         in pure $ if i < BS.length (cartRom c) then BS.index (cartRom c) i else 0xFF
    | addr >= 0xA000 && addr <= 0xBFFF =
        let i = fromIntegral (addr - 0xA000)
         in if i < MV.length (cartRam c)
                then MV.read (cartRam c) i
                else pure 0xFF
    | otherwise = pure 0xFF

noMbcWrite :: Word16 -> Word8 -> Cartridge -> IO ()
noMbcWrite addr v c
    | addr >= 0xA000 && addr <= 0xBFFF =
        let i = fromIntegral (addr - 0xA000)
         in when (i < MV.length (cartRam c)) (MV.write (cartRam c) i v)
    | otherwise = pure ()

----------------------------------------------------------------------
-- MBC1
----------------------------------------------------------------------

mbc1Read :: Mbc1State -> Word16 -> Cartridge -> IO Word8
mbc1Read s addr c
    | addr <= 0x3FFF =
        let !bankShift = if m1Multicart s then 4 else 5
            !bank =
                if m1Mode s
                    then fromIntegral (m1BankHi s) `shiftL` bankShift :: Int
                    else 0
            -- Mask the bank by the cart's actual ROM-bank count so a
            -- write of bank-hi bits beyond the cart's range wraps
            -- around, matching real-hardware MBC1 (the bus only wires
            -- as many bank-select lines as the cart ROM uses).
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral addr
         in pure (romIndex (cartRom c) off)
    | addr <= 0x7FFF =
        let !bank = mbc1HighBank s
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral (addr - 0x4000)
         in pure (romIndex (cartRom c) off)
    | addr >= 0xA000 && addr <= 0xBFFF =
        if not (m1RamEnabled s)
            then pure 0xFF
            else
                let !bank =
                        if m1Mode s
                            then fromIntegral (m1BankHi s) :: Int
                            else 0
                    !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
                 in if off < MV.length (cartRam c)
                        then MV.read (cartRam c) off
                        else pure 0xFF
    | otherwise = pure 0xFF

mbc1Write :: Mbc1State -> Word16 -> Word8 -> Cartridge -> IO ()
mbc1Write s addr v c
    | addr <= 0x1FFF =
        writeIORef (cartImpl c) (Mbc1Impl s{m1RamEnabled = (v .&. 0x0F) == 0x0A})
    | addr <= 0x3FFF =
        -- Store the raw 5-bit value. The bank-0-alias adjustment
        -- (writes of 0x00, 0x20, 0x40, 0x60 select banks 1, 0x21,
        -- 0x41, 0x61 respectively) is applied at read time inside
        -- 'mbc1HighBank', because MBC1M wires the alias check
        -- differently from standard MBC1.
        writeIORef (cartImpl c) (Mbc1Impl s{m1RomBankLow = v .&. 0x1F})
    | addr <= 0x5FFF =
        writeIORef (cartImpl c) (Mbc1Impl s{m1BankHi = v .&. 0x03})
    | addr <= 0x7FFF =
        writeIORef (cartImpl c) (Mbc1Impl s{m1Mode = testBit v 0})
    | addr >= 0xA000 && addr <= 0xBFFF && m1RamEnabled s =
        let !bank =
                if m1Mode s
                    then fromIntegral (m1BankHi s) :: Int
                    else 0
            !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
         in when (off < MV.length (cartRam c)) (MV.write (cartRam c) off v)
    | otherwise = pure ()

mbc1HighBank :: Mbc1State -> Int
mbc1HighBank s
    | m1Multicart s =
        -- MBC1M wiring: only 4 bits of bank-low are wired to the ROM,
        -- so the high nibble of bank-low does NOT contribute to the
        -- bank index; it only feeds the bank-0-alias check. Per
        -- SameBoy 'mbc.c' lines 65-77: combined =
        -- @(bank_low & 0xF) | (bank_high << 4)@ and we increment by 1
        -- when the FULL 5-bit @bank_low@ is zero (covering writes of
        -- 0x00 only — 0x10 keeps the high bit so the alias does not
        -- trigger).
        let !low4 = fromIntegral (m1RomBankLow s) .&. 0x0F :: Int
            !aliasZero = (m1RomBankLow s .&. 0x1F) == 0
            !combined = (fromIntegral (m1BankHi s) `shiftL` 4) .|. low4
         in if aliasZero then combined + 1 else combined
    | otherwise =
        let !combined = (m1BankHi s `shiftL` 5) .|. m1RomBankLow s
            !adjusted =
                if (combined .&. 0x1F) == 0
                    then combined .|. 0x01
                    else combined
         in fromIntegral adjusted

{- | Mask used to wrap an MBC1 ROM-bank index to the cart's actual
size. Real-hardware MBC1 only wires as many bank-select lines as the
cart needs, so writes beyond the cart's bank range wrap mod the bank
count. Mooneye 'emulator-only/mbc1/rom_*Mb' tests check this.
-}
mbc1BankMask :: Cartridge -> Int
mbc1BankMask c =
    let !banks = max 1 (BS.length (cartRom c) `div` 0x4000)
     in banks - 1

romIndex :: ByteString -> Int -> Word8
romIndex rom i =
    if i < BS.length rom then BS.index rom i else 0xFF

----------------------------------------------------------------------
-- MBC2
----------------------------------------------------------------------

mbc2Read :: Mbc2State -> Word16 -> Cartridge -> IO Word8
mbc2Read s addr c
    | addr <= 0x3FFF = pure (romIndex (cartRom c) (fromIntegral addr))
    | addr <= 0x7FFF =
        let !bank = fromIntegral (m2RomBank s) :: Int
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral (addr - 0x4000)
         in pure (romIndex (cartRom c) off)
    | addr >= 0xA000 && addr <= 0xBFFF =
        if not (m2RamEnabled s)
            then pure 0xFF
            else do
                -- MBC2 RAM is 512 nibbles; addresses mirror every 0x200.
                let i = fromIntegral (addr - 0xA000) `mod` 512
                v <- MV.read (cartRam c) i
                pure (0xF0 .|. (v .&. 0x0F))
    | otherwise = pure 0xFF

{- | MBC2 register writes use bit 8 of the address to disambiguate:

* @0x0000-0x3FFF@ with @addr & 0x100 == 0@: RAM enable (low nibble == 0xA).
* @0x0000-0x3FFF@ with @addr & 0x100 != 0@: ROM bank select (low 4 bits;
  zero is treated as one).
-}
mbc2Write :: Mbc2State -> Word16 -> Word8 -> Cartridge -> IO ()
mbc2Write s addr v c
    | addr <= 0x3FFF =
        if testBit addr 8
            then
                let !bank = v .&. 0x0F
                    !adj = if bank == 0 then 1 else bank
                 in writeIORef (cartImpl c) (Mbc2Impl s{m2RomBank = adj})
            else writeIORef (cartImpl c) (Mbc2Impl s{m2RamEnabled = (v .&. 0x0F) == 0x0A})
    | addr >= 0xA000 && addr <= 0xBFFF && m2RamEnabled s =
        let i = fromIntegral (addr - 0xA000) `mod` 512
         in MV.write (cartRam c) i (v .&. 0x0F)
    | otherwise = pure ()

----------------------------------------------------------------------
-- MBC3
----------------------------------------------------------------------

mbc3Read :: Mbc3State -> Word16 -> Cartridge -> IO Word8
mbc3Read s addr c
    | addr <= 0x3FFF = pure (romIndex (cartRom c) (fromIntegral addr))
    | addr <= 0x7FFF =
        let !bank = fromIntegral (m3RomBank s) :: Int
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral (addr - 0x4000)
         in pure (romIndex (cartRom c) off)
    | addr >= 0xA000 && addr <= 0xBFFF =
        if not (m3RamRtcEnabled s)
            then pure 0xFF
            else case m3RamBankOrRtc s of
                b
                    | b < 4 ->
                        let !bank = fromIntegral b :: Int
                            !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
                         in if off < MV.length (cartRam c)
                                then MV.read (cartRam c) off
                                else pure 0xFF
                b
                    | b >= 0x08 && b <= 0x0C ->
                        pure (rtcRegByte b (m3RtcLatched s))
                _ -> pure 0xFF
    | otherwise = pure 0xFF

mbc3Write :: Mbc3State -> Word16 -> Word8 -> Cartridge -> IO ()
mbc3Write s addr v c
    | addr <= 0x1FFF =
        writeIORef (cartImpl c) (Mbc3Impl s{m3RamRtcEnabled = (v .&. 0x0F) == 0x0A})
    | addr <= 0x3FFF =
        let !bank = v .&. 0x7F
            !adj = if bank == 0 then 1 else bank
         in writeIORef (cartImpl c) (Mbc3Impl s{m3RomBank = adj})
    | addr <= 0x5FFF =
        writeIORef (cartImpl c) (Mbc3Impl s{m3RamBankOrRtc = v})
    | addr <= 0x7FFF = do
        s' <-
            if m3LatchPrev s == 0x00 && v == 0x01
                then latchRtc s
                else pure s
        writeIORef (cartImpl c) (Mbc3Impl s'{m3LatchPrev = v})
    | addr >= 0xA000 && addr <= 0xBFFF && m3RamRtcEnabled s =
        case m3RamBankOrRtc s of
            b
                | b < 4 ->
                    let !bank = fromIntegral b :: Int
                        !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
                     in when (off < MV.length (cartRam c)) (MV.write (cartRam c) off v)
            b | b >= 0x08 && b <= 0x0C -> do
                s' <- writeRtcReg b v s
                writeIORef (cartImpl c) (Mbc3Impl s')
            _ -> pure ()
    | otherwise = pure ()

----------------------------------------------------------------------
-- MBC3 RTC helpers
----------------------------------------------------------------------

{- | Live elapsed seconds: when halted, the frozen base; otherwise base
plus wall-clock delta since the anchor.
-}
liveRtcSec :: Mbc3State -> IO Integer
liveRtcSec s
    | m3RtcHalted s = pure (m3RtcSecBase s)
    | otherwise = do
        now <- nowPosix
        pure (m3RtcSecBase s + (now - m3RtcAnchor s))

{- | Decompose the live RTC into the five registers (with halt and
day-carry bits already merged into DH).
-}
liveRtcRegs :: Mbc3State -> IO RtcRegs
liveRtcRegs s = do
    e <- liveRtcSec s
    pure (composeRegs e (m3RtcHalted s) (m3RtcDayCarry s))

composeRegs :: Integer -> Bool -> Bool -> RtcRegs
composeRegs total halted carry =
    let safe = max 0 total
        sec = safe `mod` 60
        mn = (safe `div` 60) `mod` 60
        hr = (safe `div` 3600) `mod` 24
        days = safe `div` 86400
        days9 = days `mod` 512
        dl = fromIntegral (days9 `mod` 256) :: Word8
        dayHi = days9 >= 256
        dh =
            (if dayHi then 0x01 else 0x00)
                .|. (if halted then 0x40 else 0x00)
                .|. (if carry then 0x80 else 0x00)
     in RtcRegs (fromIntegral sec) (fromIntegral mn) (fromIntegral hr) dl dh

{- | Reverse of 'composeRegs': pack the five regs back into an elapsed
second count, ignoring halt/carry bits in DH.
-}
decomposeRegs :: RtcRegs -> Integer
decomposeRegs r =
    let dayHi = testBit (rrDH r) 0
        days = fromIntegral (rrDL r) + (if dayHi then 256 else 0) :: Integer
     in fromIntegral (rrS r)
            + 60 * fromIntegral (rrM r)
            + 3600 * fromIntegral (rrH r)
            + 86400 * days

{- | Latch the live RTC into 'm3RtcLatched'. Called on a 0x00 -> 0x01
transition of the byte written to 0x6000-0x7FFF.
-}
latchRtc :: Mbc3State -> IO Mbc3State
latchRtc s = do
    regs <- liveRtcRegs s
    pure s{m3RtcLatched = regs}

{- | Bus write to one of the five RTC regs. Updates the live state and
mirrors the value into the latched copy so the next read sees it
without a fresh latch.
-}
writeRtcReg :: Word8 -> Word8 -> Mbc3State -> IO Mbc3State
writeRtcReg sel v s = do
    live <- liveRtcRegs s
    let live' = setReg sel v live
        newHalt = testBit (rrDH live') 6
        newCarry = testBit (rrDH live') 7
        newSec = decomposeRegs live'
    now <- nowPosix
    let latched' = setReg sel v (m3RtcLatched s)
    pure
        s
            { m3RtcSecBase = newSec
            , m3RtcAnchor = now
            , m3RtcHalted = newHalt
            , m3RtcDayCarry = newCarry
            , m3RtcLatched = latched'
            }

setReg :: Word8 -> Word8 -> RtcRegs -> RtcRegs
setReg 0x08 v r = r{rrS = v .&. 0x3F}
setReg 0x09 v r = r{rrM = v .&. 0x3F}
setReg 0x0A v r = r{rrH = v .&. 0x1F}
setReg 0x0B v r = r{rrDL = v}
setReg 0x0C v r = r{rrDH = v .&. 0xC1}
setReg _ _ r = r

rtcRegByte :: Word8 -> RtcRegs -> Word8
rtcRegByte 0x08 r = rrS r
rtcRegByte 0x09 r = rrM r
rtcRegByte 0x0A r = rrH r
rtcRegByte 0x0B r = rrDL r
rtcRegByte 0x0C r = rrDH r
rtcRegByte _ _ = 0xFF

{- | Size of the RTC suffix appended to @.sav@ files: 10 little-endian
@uint32@s (live + latched S/M/H/DL/DH) plus a little-endian @int64@
timestamp.
-}
rtcSuffixSize :: Int
rtcSuffixSize = 48

{- | Encode the cart's RTC into a 48-byte suffix, or 'BS.empty' for
carts without an RTC.
-}
extractRtcSuffix :: Cartridge -> IO ByteString
extractRtcSuffix c = do
    impl <- readIORef (cartImpl c)
    case impl of
        Mbc3Impl s | capTimer (hdrCaps (cartHeader c)) -> do
            live <- liveRtcRegs s
            now <- nowPosix
            let bb =
                    encodeRtcRegs live
                        <> encodeRtcRegs (m3RtcLatched s)
                        <> BB.int64LE (fromIntegral now)
            pure (BL.toStrict (BB.toLazyByteString bb))
        _ -> pure BS.empty

encodeRtcRegs :: RtcRegs -> BB.Builder
encodeRtcRegs r =
    BB.word32LE (fromIntegral (rrS r))
        <> BB.word32LE (fromIntegral (rrM r))
        <> BB.word32LE (fromIntegral (rrH r))
        <> BB.word32LE (fromIntegral (rrDL r))
        <> BB.word32LE (fromIntegral (rrDH r))

{- | Apply a 48-byte RTC suffix to the cart's state, advancing the live
counter by @now - savedTime@ seconds when the saved RTC was running.
Carts without an RTC ignore the suffix.
-}
applyRtcSuffix :: ByteString -> Cartridge -> IO ()
applyRtcSuffix bs c = do
    impl <- readIORef (cartImpl c)
    case impl of
        Mbc3Impl s | capTimer (hdrCaps (cartHeader c)) -> do
            let live = decodeRtcRegs (BS.take 20 bs)
                latched = decodeRtcRegs (BS.take 20 (BS.drop 20 bs))
                savedTime = fromIntegral (decodeI64LE (BS.drop 40 bs)) :: Integer
                halted = testBit (rrDH live) 6
                carry = testBit (rrDH live) 7
                savedSec = decomposeRegs live
            now <- nowPosix
            let elapsed = if halted then 0 else max 0 (now - savedTime)
                newSec = savedSec + elapsed
            writeIORef
                (cartImpl c)
                ( Mbc3Impl
                    s
                        { m3RtcSecBase = newSec
                        , m3RtcAnchor = now
                        , m3RtcHalted = halted
                        , m3RtcDayCarry = carry
                        , m3RtcLatched = latched
                        }
                )
        _ -> pure ()

decodeRtcRegs :: ByteString -> RtcRegs
decodeRtcRegs bs =
    RtcRegs
        { rrS = fromIntegral (decodeU32LE (BS.take 4 bs))
        , rrM = fromIntegral (decodeU32LE (BS.take 4 (BS.drop 4 bs)))
        , rrH = fromIntegral (decodeU32LE (BS.take 4 (BS.drop 8 bs)))
        , rrDL = fromIntegral (decodeU32LE (BS.take 4 (BS.drop 12 bs)))
        , rrDH = fromIntegral (decodeU32LE (BS.take 4 (BS.drop 16 bs)))
        }

decodeU32LE :: ByteString -> Word32
decodeU32LE bs =
    fromIntegral (BS.index bs 0)
        .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
        .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
        .|. (fromIntegral (BS.index bs 3) `shiftL` 24)

decodeI64LE :: ByteString -> Int64
decodeI64LE bs =
    let b i = fromIntegral (BS.index bs i) :: Int64
     in b 0
            .|. (b 1 `shiftL` 8)
            .|. (b 2 `shiftL` 16)
            .|. (b 3 `shiftL` 24)
            .|. (b 4 `shiftL` 32)
            .|. (b 5 `shiftL` 40)
            .|. (b 6 `shiftL` 48)
            .|. (b 7 `shiftL` 56)

----------------------------------------------------------------------
-- MBC5
----------------------------------------------------------------------

mbc5Read :: Mbc5State -> Word16 -> Cartridge -> IO Word8
mbc5Read s addr c
    | addr <= 0x3FFF = pure (romIndex (cartRom c) (fromIntegral addr))
    | addr <= 0x7FFF =
        let !bank = mbc5HighBank s
            -- Real-hardware MBC5 only wires as many bank-select lines
            -- as the cart needs; out-of-range bank indices wrap mod
            -- the cart's bank count (matches SameBoy's @& (rom_size -
            -- 1)@). Mooneye 'emulator-only/mbc5/rom_*Mb' verify this.
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral (addr - 0x4000)
         in pure (romIndex (cartRom c) off)
    | addr >= 0xA000 && addr <= 0xBFFF =
        if not (m5RamEnabled s)
            then pure 0xFF
            else
                let !bank = fromIntegral (m5RamBank s) :: Int
                    !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
                 in if off < MV.length (cartRam c)
                        then MV.read (cartRam c) off
                        else pure 0xFF
    | otherwise = pure 0xFF

mbc5Write :: Mbc5State -> Word16 -> Word8 -> Cartridge -> IO ()
mbc5Write s addr v c
    | addr <= 0x1FFF =
        writeIORef (cartImpl c) (Mbc5Impl s{m5RamEnabled = (v .&. 0x0F) == 0x0A})
    | addr <= 0x2FFF =
        writeIORef (cartImpl c) (Mbc5Impl s{m5RomBankLow = v})
    | addr <= 0x3FFF =
        writeIORef (cartImpl c) (Mbc5Impl s{m5RomBankHigh = v .&. 0x01})
    | addr <= 0x5FFF =
        writeIORef (cartImpl c) (Mbc5Impl s{m5RamBank = v .&. 0x0F})
    | addr <= 0x7FFF = pure ()
    | addr >= 0xA000 && addr <= 0xBFFF && m5RamEnabled s =
        let !bank = fromIntegral (m5RamBank s) :: Int
            !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
         in when (off < MV.length (cartRam c)) (MV.write (cartRam c) off v)
    | otherwise = pure ()

mbc5HighBank :: Mbc5State -> Int
mbc5HighBank s =
    (fromIntegral (m5RomBankHigh s) `shiftL` 8) .|. fromIntegral (m5RomBankLow s)

----------------------------------------------------------------------
-- HuC1
----------------------------------------------------------------------

huc1Read :: HuC1State -> Word16 -> Cartridge -> IO Word8
huc1Read s addr c
    | addr <= 0x3FFF = pure (romIndex (cartRom c) (fromIntegral addr))
    | addr <= 0x7FFF =
        let !bank = fromIntegral (hcRomBank s) :: Int
            !off = (bank .&. mbc1BankMask c) * 0x4000 + fromIntegral (addr - 0x4000)
         in pure (romIndex (cartRom c) off)
    | addr >= 0xA000 && addr <= 0xBFFF =
        if not (hcRamEnabled s)
            then pure 0xC0 -- Approximation of IR-mode reads (no IR data).
            else
                let !bank = fromIntegral (hcRamBank s) :: Int
                    !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
                 in if off < MV.length (cartRam c)
                        then MV.read (cartRam c) off
                        else pure 0xFF
    | otherwise = pure 0xFF

huc1Write :: HuC1State -> Word16 -> Word8 -> Cartridge -> IO ()
huc1Write s addr v c
    | addr <= 0x1FFF =
        -- 0x0E selects IR mode; anything else (notably 0x0A) selects RAM.
        writeIORef (cartImpl c) (HuC1Impl s{hcRamEnabled = (v .&. 0x0F) /= 0x0E})
    | addr <= 0x3FFF =
        writeIORef (cartImpl c) (HuC1Impl s{hcRomBank = v .&. 0x3F})
    | addr <= 0x5FFF =
        writeIORef (cartImpl c) (HuC1Impl s{hcRamBank = v .&. 0x03})
    | addr <= 0x7FFF = pure ()
    | addr >= 0xA000 && addr <= 0xBFFF && hcRamEnabled s =
        let !bank = fromIntegral (hcRamBank s) :: Int
            !off = bank * 0x2000 + fromIntegral (addr - 0xA000)
         in when (off < MV.length (cartRam c)) (MV.write (cartRam c) off v)
    | otherwise = pure ()
