{-# LANGUAGE BangPatterns #-}

{- | Tiny put/get primitives for the snapshot format.

The snapshot is a flat byte-oriented blob: each section knows its own
shape, so there are no tagged unions or self-describing types. All
multi-byte values are little-endian.

For variable-size blobs (RAM, VRAM, etc.) the encoder emits a 32-bit
length prefix followed by the payload, which lets the decoder advance a
running cursor without knowing payload sizes in advance.

The cursor is bounds-checked: reading past the end of the buffer yields
zero bytes and latches an overrun flag rather than throwing. Callers that
care use 'runCursorChecked', which turns a latched overrun into 'Nothing'.
This matters because the decoded values are handed to lazy record fields,
so an out-of-range 'BS.index' would otherwise survive as a thunk and blow
up somewhere far away from the decode site.
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
    runCursorChecked,
    cursorBytes,
    getU8,
    getU16,
    getU32,
    getI64,
    getBool,
    getBlob,
    getFixed,
) where

import Control.Monad (when)
import Control.Monad.Trans.State.Strict (State, evalState, get, put, runState)
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

{- | Read-cursor state: the buffer, the current offset, and whether any read
so far has run past the end of the buffer.
-}
data CursorState = CursorState
    { csBuffer :: !ByteString
    , csOffset :: !Int
    , csOverrun :: !Bool
    }

-- | A read cursor over a 'ByteString'.
type Cursor = State CursorState

{- | Run a cursor action against a buffer, ignoring overrun. Reads past the
end produce zero bytes. Use this only where the payload length is already
known to be right (e.g. a blob the outer decoder has already framed).
-}
runCursor :: Cursor a -> ByteString -> a
runCursor m bs = evalState m (CursorState bs 0 False)

{- | Run a cursor action and return 'Nothing' if any read ran past the end of
the buffer. The result is discarded on overrun, so a short buffer can never
leak a zero-filled value into the caller.
-}
runCursorChecked :: Cursor a -> ByteString -> Maybe a
runCursorChecked m bs =
    let (a, st) = runState m (CursorState bs 0 False)
     in if csOverrun st then Nothing else Just a

-- | Number of bytes consumed so far.
cursorBytes :: Cursor Int
cursorBytes = csOffset <$> get

advance :: Int -> Cursor ()
advance n = do
    st <- get
    put st{csOffset = csOffset st + n}

-- | Latch the overrun flag. Subsequent reads still run; they just yield zeros.
markOverrun :: Cursor ()
markOverrun = do
    st <- get
    put st{csOverrun = True}

peekByte :: Int -> Cursor Word8
peekByte i = do
    st <- get
    let !ix = csOffset st + i
    if ix >= 0 && ix < BS.length (csBuffer st)
        then pure (BS.index (csBuffer st) ix)
        else do
            markOverrun
            pure 0

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

{- | Read a length-prefixed blob. A prefix that promises more bytes than the
buffer holds is an overrun, not a silently short payload.
-}
getBlob :: Cursor ByteString
getBlob = do
    n <- fromIntegral <$> getU32
    getFixed n

{- | Read a fixed-size run of bytes, latching an overrun if fewer than @n@
remain.
-}
getFixed :: Int -> Cursor ByteString
getFixed n = do
    st <- get
    let payload = BS.take n (BS.drop (csOffset st) (csBuffer st))
    advance n
    when (BS.length payload < n) markOverrun
    pure payload
