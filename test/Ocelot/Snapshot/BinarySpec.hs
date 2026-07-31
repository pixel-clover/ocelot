{-# LANGUAGE ScopedTypeVariables #-}

module Ocelot.Snapshot.BinarySpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word16, Word32, Word8)
import Ocelot.Snapshot.Binary
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- QuickCheck's 'Fixed' modifier exports a 'getFixed' accessor that collides with
-- the cursor primitive of the same name.
import Test.QuickCheck hiding (getFixed)

render :: BB.Builder -> ByteString
render = BL.toStrict . BB.toLazyByteString

{- | QuickCheck ships no 'Arbitrary' for 'ByteString', and one orphan instance
would be more than this module needs.
-}
byteStrings :: Gen ByteString
byteStrings = BS.pack <$> arbitrary

spec :: Spec
spec = do
    {- These primitives sit under every save state, and the browser hands them
    blobs from IndexedDB and from files a user picked, so they parse untrusted
    bytes. A getter that silently zero-fills where it should signal an overrun is
    how a corrupt blob becomes a corrupt machine instead of a rejected load. -}
    describe "put/get round-trips" $ do
        prop "putU8 then getU8" $ \(w :: Word8) ->
            runCursor getU8 (render (putU8 w)) === w

        prop "putU16 then getU16" $ \(w :: Word16) ->
            runCursor getU16 (render (putU16 w)) === w

        prop "putU32 then getU32" $ \(w :: Word32) ->
            runCursor getU32 (render (putU32 w)) === w

        prop "putI64 then getI64, including negatives" $ \(i :: Int64) ->
            runCursor getI64 (render (putI64 i)) === i

        prop "putBool then getBool" $ \b ->
            runCursor getBool (render (putBool b)) === b

        prop "putBlob then getBlob" $ forAll byteStrings $ \bs ->
            runCursor getBlob (render (putBlob bs)) === bs

        prop "a heterogeneous record round-trips in field order" $
            \(w8 :: Word8) (w16 :: Word16) (w32 :: Word32) (i :: Int64) b ->
                forAll byteStrings $ \bs ->
                    let blob =
                            render
                                ( putU8 w8
                                    <> putU16 w16
                                    <> putU32 w32
                                    <> putI64 i
                                    <> putBool b
                                    <> putBlob bs
                                )
                        decode = do
                            a1 <- getU8
                            a2 <- getU16
                            a3 <- getU32
                            a4 <- getI64
                            a5 <- getBool
                            a6 <- getBlob
                            pure (a1, a2, a3, a4, a5, a6)
                     in runCursorChecked decode blob
                            === Just (w8, w16, w32, i, b, bs)

    describe "cursorBytes" $ do
        prop "reports exactly the number of bytes the getters consumed" $
            \(w8 :: Word8) (w32 :: Word32) ->
                forAll byteStrings $ \bs ->
                    let blob = render (putU8 w8 <> putU32 w32 <> putBlob bs)
                        decode = getU8 >> getU32 >> getBlob >> cursorBytes
                     in runCursor decode blob === BS.length blob

    describe "runCursorChecked" $ do
        prop "accepts a buffer of exactly the right length" $ \(w32 :: Word32) ->
            runCursorChecked getU32 (render (putU32 w32)) === Just w32

        prop "rejects a buffer one byte short" $ \(w32 :: Word32) ->
            runCursorChecked getU32 (BS.init (render (putU32 w32))) === Nothing

        it "rejects an empty buffer" $
            runCursorChecked getU8 BS.empty `shouldBe` Nothing

        it "rejects a blob whose length prefix promises more than the buffer holds" $ do
            -- Prefix says four bytes; only two follow.
            let blob = render (putU32 4 <> putU8 0xAA <> putU8 0xBB)
            runCursorChecked getBlob blob `shouldBe` Nothing

        it "rejects a getFixed that runs past the end" $
            runCursorChecked (getFixed 4) (BS.pack [1, 2]) `shouldBe` Nothing

        prop "accepts trailing bytes it was never asked to read" $ \(w8 :: Word8) ->
            forAll byteStrings $ \extra ->
                runCursorChecked getU8 (render (putU8 w8) <> extra) === Just w8

    describe "runCursor" $ do
        {- The lenient variant is only valid where an outer decoder has already
        framed the payload, so its contract is to zero-fill rather than fail. That
        is worth pinning: it is the difference between the two entry points, and
        picking the wrong one for untrusted bytes is the mistake to avoid. -}
        it "zero-fills reads past the end instead of failing" $ do
            runCursor getU8 BS.empty `shouldBe` 0
            runCursor getU16 BS.empty `shouldBe` 0
            runCursor getU32 BS.empty `shouldBe` 0
            runCursor getI64 BS.empty `shouldBe` 0
            runCursor getBool BS.empty `shouldBe` False

        it "returns a short payload for a getFixed that overruns" $
            runCursor (getFixed 4) (BS.pack [1, 2]) `shouldBe` BS.pack [1, 2]

        prop "agrees with runCursorChecked whenever the buffer is long enough" $
            \(w32 :: Word32) ->
                let blob = render (putU32 w32)
                 in Just (runCursor getU32 blob) === runCursorChecked getU32 blob
