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
    rlc8,
    rrc8,
    rl8,
    rr8,
    sla8,
    sra8,
    swap8,
    srl8,
    bit8,
    daa,
) where

import Data.Bits (rotate, shiftL, shiftR, testBit, xor, (.&.), (.|.))
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

{- | Rotate left through carry's mirror: @bit 7@ wraps to @bit 0@ and into C.
The CB-prefix variant; the @RLCA@ form forces Z to 'False' separately.
-}
rlc8 :: Word8 -> (Word8, Flags)
rlc8 v =
    let !r = rotate v 1
        !c = testBit v 7
     in (r, Flags (r == 0) False False c)

-- | Rotate right circular.
rrc8 :: Word8 -> (Word8, Flags)
rrc8 v =
    let !r = rotate v (-1)
        !c = testBit v 0
     in (r, Flags (r == 0) False False c)

{- | Rotate left through C: bit 7 -> C; old C -> bit 0. The @RLA@ form forces Z
to 'False' in the executor.
-}
rl8 :: Word8 -> Bool -> (Word8, Flags)
rl8 v cIn =
    let !cOut = testBit v 7
        !r = (v `shiftL` 1) .|. (if cIn then 1 else 0)
     in (r, Flags (r == 0) False False cOut)

-- | Rotate right through C: bit 0 -> C; old C -> bit 7.
rr8 :: Word8 -> Bool -> (Word8, Flags)
rr8 v cIn =
    let !cOut = testBit v 0
        !r = (v `shiftR` 1) .|. (if cIn then 0x80 else 0)
     in (r, Flags (r == 0) False False cOut)

{- | Shift left: bit 7 -> C; bit 0 cleared. (On the SM83, SLA is the only
left-shift form; no SLL.)
-}
sla8 :: Word8 -> (Word8, Flags)
sla8 v =
    let !c = testBit v 7
        !r = v `shiftL` 1
     in (r, Flags (r == 0) False False c)

-- | Arithmetic shift right: bit 0 -> C; bit 7 preserved.
sra8 :: Word8 -> (Word8, Flags)
sra8 v =
    let !c = testBit v 0
        !msb = v .&. 0x80
        !r = (v `shiftR` 1) .|. msb
     in (r, Flags (r == 0) False False c)

-- | Swap nibbles. C/N/H all cleared.
swap8 :: Word8 -> (Word8, Flags)
swap8 v =
    let !r = (v `shiftL` 4) .|. (v `shiftR` 4)
     in (r, Flags (r == 0) False False False)

-- | Logical shift right: bit 0 -> C; bit 7 cleared.
srl8 :: Word8 -> (Word8, Flags)
srl8 v =
    let !c = testBit v 0
        !r = v `shiftR` 1
     in (r, Flags (r == 0) False False c)

{- | @BIT b, n@. Z := !bit b; N := 0; H := 1; C preserved (passed in).
Bit numbers outside @0..7@ produce 'False' from 'testBit', matching the
SM83's behavior on a hypothetical out-of-range @b@ (only @0..7@ are encodable).
-}
bit8 :: Int -> Word8 -> Bool -> Flags
bit8 b v = Flags (not (testBit v b)) False True

{- | @DAA@: BCD-adjust A using the prior N, H, C flags. The N flag is preserved;
H is always cleared; C is updated only on the addition path.
-}
daa :: Word8 -> Bool -> Bool -> Bool -> (Word8, Flags)
daa a n h c =
    let !(!r, !cOut) =
            if not n
                then
                    let !aL = if h || (a .&. 0x0F) > 0x09 then a + 0x06 else a
                        !carry = c || a > 0x99
                        !aH = if carry then aL + 0x60 else aL
                     in (aH, carry)
                else
                    let !aL = if h then a - 0x06 else a
                        !aH = if c then aL - 0x60 else aL
                     in (aH, c)
     in (r, Flags (r == 0) n False cOut)
