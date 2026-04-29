{- | Tiny put/get primitives for the snapshot format.

The snapshot is a flat byte-oriented blob: each section knows its own
shape, so there are no tagged unions or self-describing types. All
multi-byte values are little-endian.

For variable-size blobs (RAM, VRAM, etc.) the encoder emits a 32-bit
length prefix followed by the payload, which lets the decoder advance a
running cursor without knowing payload sizes in advance.
-}
module Ocelot.Snapshot.Binary (
    -- * Builder side
    putU8,
    putU16,
    putU32,
    putI64,
    putBool,
    putBlob,

    -- * Cursor side
    Cursor,
    runCursor,
    cursorBytes,
    getU8,
    getU16,
    getU32,
    getI64,
    getBool,
    getBlob,
    getFixed,
) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.Int (Int64)
import Data.Word (Word16, Word32, Word8)

----------------------------------------------------------------------
-- Builder side
----------------------------------------------------------------------

putU8 :: Word8 -> BB.Builder
putU8 = BB.word8

putU16 :: Word16 -> BB.Builder
putU16 = BB.word16LE

putU32 :: Word32 -> BB.Builder
putU32 = BB.word32LE

putI64 :: Int64 -> BB.Builder
putI64 = BB.int64LE

putBool :: Bool -> BB.Builder
putBool b = BB.word8 (if b then 1 else 0)

-- | Length-prefixed byte string: 32-bit LE length, then the payload.
putBlob :: ByteString -> BB.Builder
putBlob bs = BB.word32LE (fromIntegral (BS.length bs)) <> BB.byteString bs

----------------------------------------------------------------------
-- Cursor side
----------------------------------------------------------------------

-- | A read cursor over a 'ByteString': @(buffer, offset)@.
type Cursor = State (ByteString, Int)

-- | Run a cursor action against a buffer.
runCursor :: Cursor a -> ByteString -> a
runCursor m bs = evalState m (bs, 0)

-- | Number of bytes consumed so far.
cursorBytes :: Cursor Int
cursorBytes = do
    (_, off) <- get
    pure off

advance :: Int -> Cursor ()
advance n = do
    (bs, off) <- get
    put (bs, off + n)

peekByte :: Int -> Cursor Word8
peekByte i = do
    (bs, off) <- get
    pure (BS.index bs (off + i))

getU8 :: Cursor Word8
getU8 = do
    b <- peekByte 0
    advance 1
    pure b

getU16 :: Cursor Word16
getU16 = do
    b0 <- peekByte 0
    b1 <- peekByte 1
    advance 2
    pure (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8))

getU32 :: Cursor Word32
getU32 = do
    let bf i = fromIntegral <$> peekByte i :: Cursor Word32
    b0 <- bf 0
    b1 <- bf 1
    b2 <- bf 2
    b3 <- bf 3
    advance 4
    pure (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24))

getI64 :: Cursor Int64
getI64 = do
    let bf i = fromIntegral <$> peekByte i :: Cursor Int64
    bs <- mapM bf [0 .. 7]
    advance 8
    pure $
        foldr
            (\(s, b) acc -> acc .|. (b `shiftL` s))
            0
            (zip [0, 8, 16, 24, 32, 40, 48, 56] bs)

getBool :: Cursor Bool
getBool = (/= 0) <$> getU8

-- | Read a length-prefixed blob.
getBlob :: Cursor ByteString
getBlob = do
    n <- fromIntegral <$> getU32
    (bs, off) <- get
    let payload = BS.take n (BS.drop off bs)
    advance n
    pure payload

-- | Read a fixed-size run of bytes.
getFixed :: Int -> Cursor ByteString
getFixed n = do
    (bs, off) <- get
    let payload = BS.take n (BS.drop off bs)
    advance n
    pure payload
