{-# LANGUAGE BangPatterns #-}

{- | SM83 arithmetic and logic unit.

Pure functions over 8-bit and 16-bit operands. Each function returns the
operation result and the resulting Z/N/H/C flag values; they do not touch any
register state. The executor calls these and folds the results back through
'Ocelot.Cpu.Registers'.

Reference: pandocs CPU instruction set
(<https://gbdev.io/pandocs/CPU_Instruction_Set.html>) and the gbdev opcode
table (<https://gbdev.io/gb-opcodes/optables/>).

For 'inc8', 'dec8', and 'add16', the previous value of one or more flags is
preserved on the SM83 ('inc8' and 'dec8' preserve C, 'add16' preserves Z).
Those functions take that flag as an explicit argument so they remain pure
and free of any 'Registers' coupling.
-}
module Ocelot.Cpu.Alu (
    Flags (..),
    add8,
    adc8,
    sub8,
    sbc8,
    and8,
    or8,
    xor8,
    cp8,
    inc8,
    dec8,
    add16,
    addSP,
) where

import Data.Bits (xor, (.&.), (.|.))
import Data.Int (Int8)
import Data.Word (Word16, Word32, Word8)

data Flags = Flags
    { flagZ :: !Bool
    , flagN :: !Bool
    , flagH :: !Bool
    , flagC :: !Bool
    }
    deriving (Eq, Show)

-- | @ADD A, n@. N = 0; H = carry from bit 3; C = carry from bit 7.
add8 :: Word8 -> Word8 -> (Word8, Flags)
add8 a b =
    let !r = a + b
        !h = (a .&. 0x0F) + (b .&. 0x0F) > 0x0F
        !c = (fromIntegral a + fromIntegral b :: Word16) > 0xFF
     in (r, Flags (r == 0) False h c)

{- | @ADC A, n@. Adds the previous carry. Half-carry and carry are computed
across the three-operand sum.
-}
adc8 :: Word8 -> Word8 -> Bool -> (Word8, Flags)
adc8 a b cin =
    let cw :: Word8
        cw = if cin then 1 else 0
        !r = a + b + cw
        !h = (a .&. 0x0F) + (b .&. 0x0F) + cw > 0x0F
        !c = (fromIntegral a + fromIntegral b + fromIntegral cw :: Word16) > 0xFF
     in (r, Flags (r == 0) False h c)

{- | @SUB n@ (or @SUB A, n@). N = 1; H is set when borrow from bit 4 is needed;
C is set when @b > a@.
-}
sub8 :: Word8 -> Word8 -> (Word8, Flags)
sub8 a b =
    let !r = a - b
        !h = (a .&. 0x0F) < (b .&. 0x0F)
        !c = a < b
     in (r, Flags (r == 0) True h c)

-- | @SBC A, n@. Subtracts the previous carry as an additional borrow.
sbc8 :: Word8 -> Word8 -> Bool -> (Word8, Flags)
sbc8 a b cin =
    let cw :: Word8
        cw = if cin then 1 else 0
        !r = a - b - cw
        !h = (a .&. 0x0F) < (b .&. 0x0F) + cw
        -- Use Int to keep the carry comparison free of Word8 underflow.
        !c =
            (fromIntegral a :: Int)
                < (fromIntegral b :: Int) + fromIntegral cw
     in (r, Flags (r == 0) True h c)

-- | @AND n@. The H flag is unconditionally set on the SM83; C and N are clear.
and8 :: Word8 -> Word8 -> (Word8, Flags)
and8 a b =
    let !r = a .&. b
     in (r, Flags (r == 0) False True False)

-- | @OR n@. All non-Z flags are clear.
or8 :: Word8 -> Word8 -> (Word8, Flags)
or8 a b =
    let !r = a .|. b
     in (r, Flags (r == 0) False False False)

-- | @XOR n@. All non-Z flags are clear.
xor8 :: Word8 -> Word8 -> (Word8, Flags)
xor8 a b =
    let !r = a `xor` b
     in (r, Flags (r == 0) False False False)

-- | @CP n@. Same as 'sub8' but the result is discarded; only flags are kept.
cp8 :: Word8 -> Word8 -> Flags
cp8 a b = snd (sub8 a b)

{- | @INC n@. C is preserved (passed in as @cIn@); N = 0; H is set when bit 3
carries.
-}
inc8 :: Word8 -> Bool -> (Word8, Flags)
inc8 v cIn =
    let !r = v + 1
        !h = (v .&. 0x0F) == 0x0F
     in (r, Flags (r == 0) False h cIn)

{- | @DEC n@. C is preserved; N = 1; H is set when borrowing from bit 4 (i.e.
the low nibble was zero before the decrement).
-}
dec8 :: Word8 -> Bool -> (Word8, Flags)
dec8 v cIn =
    let !r = v - 1
        !h = (v .&. 0x0F) == 0x00
     in (r, Flags (r == 0) True h cIn)

{- | @ADD HL, rr@. Z is preserved (passed in as @zIn@); N = 0; H is set when
bit 11 carries; C is set when bit 15 carries.
-}
add16 :: Word16 -> Word16 -> Bool -> (Word16, Flags)
add16 a b zIn =
    let !r = a + b
        !h = (a .&. 0x0FFF) + (b .&. 0x0FFF) > 0x0FFF
        !c = (fromIntegral a + fromIntegral b :: Word32) > 0xFFFF
     in (r, Flags zIn False h c)

{- | @ADD SP, e@ where @e@ is a signed 8-bit immediate. Z = 0, N = 0; H and C
are computed from the unsigned addition of @SP@'s low byte and the immediate's
byte value (the SM83 treats the half-carry and carry as 8-bit even though the
result is 16-bit).
-}
addSP :: Word16 -> Int8 -> (Word16, Flags)
addSP sp e =
    let !signedExt = fromIntegral e :: Word16
        !eByte = fromIntegral e :: Word8
        !eU = fromIntegral eByte :: Word16
        !r = sp + signedExt
        !spLo = sp .&. 0xFF
        !h = (spLo .&. 0x0F) + (eU .&. 0x0F) > 0x0F
        !c = spLo + eU > 0xFF
     in (r, Flags False False h c)
