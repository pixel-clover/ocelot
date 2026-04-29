{-# LANGUAGE BangPatterns #-}

{- | Flat 64 KiB byte-addressable memory used by the CPU step loop while the
bus and cartridge wiring is being built.

This is a placeholder. Once 'Ocelot.Bus' exists, reads and writes will route
through cartridge ROM/RAM, VRAM, OAM, IO, and HRAM regions. Until then, the
executor exercises ALU/jump/load behavior against this flat byte map.
-}
module Ocelot.Memory (
    Memory,
    initialMemory,
    fromBytes,
    read8,
    write8,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word16, Word8)

newtype Memory = Memory {memBytes :: Vector Word8}
    deriving (Eq, Show)

initialMemory :: Memory
initialMemory = Memory (V.replicate 0x10000 0)

{- | Place the given bytes at offset 0; the rest of the 64 KiB is zero-filled.
Bytes beyond the 64 KiB cap are dropped.
-}
fromBytes :: ByteString -> Memory
fromBytes bs =
    let trimmed = BS.take 0x10000 bs
        prefix = V.fromList (BS.unpack trimmed)
        rest = V.replicate (0x10000 - V.length prefix) 0
     in Memory (prefix V.++ rest)

read8 :: Word16 -> Memory -> Word8
read8 addr (Memory v) = v V.! fromIntegral addr

write8 :: Word16 -> Word8 -> Memory -> Memory
write8 addr !v (Memory bytes) =
    Memory (bytes V.// [(fromIntegral addr, v)])
