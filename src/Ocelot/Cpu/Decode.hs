{-# LANGUAGE BangPatterns #-}

{- | SM83 instruction decoder for a deliberately small subset.

The 'Reg8' / 'Reg16' / 'Reg16Stack' / 'AluOp' enums are ordered to match the
opcode encoding so that the regular blocks (LD r,r' at @0x40-0x7F@, ALU r at
@0x80-0xBF@, etc.) decode by bit decomposition rather than 256 hand-written
cases.

Currently supported:

* NOP, HALT
* LD r,r' (every form except @LD (HL),(HL)@, whose opcode is HALT)
* LD r,d8, LD rr,d16
* LD A,(BC) / (DE) / (HL+) / (HL-); LD (BC) / (DE) / (HL+) / (HL-),A
* INC r, DEC r (8-bit, including @(HL)@), INC rr, DEC rr
* ALU op A,r and ALU op A,d8 for all eight ALU ops
* JR e, JR cc,e, JP nn, CALL nn, RET
* PUSH rr, POP rr (BC, DE, HL, AF)

Anything else decodes to @'Unknown' opcode@ so the executor can stop the run.
The CB-prefix block, conditional JP/CALL/RET, RLCA/RLA/etc., DAA, ADD HL/SP
and the high-page LDH/LD (C) forms are all deferred.
-}
module Ocelot.Cpu.Decode (
    Reg8 (..),
    Reg16 (..),
    Reg16Stack (..),
    AluOp (..),
    Cond (..),
    Instruction (..),
    Decoded (..),
    decode,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int8)
import Data.Word (Word16, Word8)

{- | 8-bit register encoding used in the @LD r,r'@ and ALU blocks. The Enum
ordering matches bits 0..2 of the opcode (B=0 .. A=7, with @(HL)@ at 6).
-}
data Reg8 = RB | RC | RD | RE | RH | RL | RIndHL | RA
    deriving (Eq, Show, Enum, Bounded)

-- | 16-bit register pair for ALU and load-immediate forms (BC=0, DE=1, HL=2, SP=3).
data Reg16 = RBC | RDE | RHL | RSP
    deriving (Eq, Show, Enum, Bounded)

-- | 16-bit register pair for stack ops; AF replaces SP at index 3.
data Reg16Stack = SBC | SDE | SHL | SAF
    deriving (Eq, Show, Enum, Bounded)

{- | ALU operation encoding from bits 3..5 of opcodes @0x80-0xBF@ (and the
@0xC6/0xCE/0xD6/0xDE/0xE6/0xEE/0xF6/0xFE@ immediate forms).
-}
data AluOp = AluAdd | AluAdc | AluSub | AluSbc | AluAnd | AluXor | AluOr | AluCp
    deriving (Eq, Show, Enum, Bounded)

data Cond = CondNZ | CondZ | CondNC | CondC
    deriving (Eq, Show, Enum, Bounded)

data Instruction
    = Nop
    | Halt
    | LdRR !Reg8 !Reg8
    | LdRD8 !Reg8 !Word8
    | LdRrD16 !Reg16 !Word16
    | LdABC
    | LdADE
    | LdBCA
    | LdDEA
    | LdAHLI
    | LdAHLD
    | LdHLIA
    | LdHLDA
    | IncR !Reg8
    | DecR !Reg8
    | IncRr !Reg16
    | DecRr !Reg16
    | AluR !AluOp !Reg8
    | AluD8 !AluOp !Word8
    | Jr !Int8
    | JrCC !Cond !Int8
    | JpNN !Word16
    | CallNN !Word16
    | Ret
    | PushRr !Reg16Stack
    | PopRr !Reg16Stack
    | Unknown !Word8
    deriving (Eq, Show)

data Decoded = Decoded
    { dInstr :: !Instruction
    , dLen :: !Int
    -- ^ 1, 2, or 3 bytes consumed (used by 'step' to advance PC).
    }
    deriving (Eq, Show)

{- | Decode a single instruction starting at the supplied opcode byte. Up to
two further bytes ('b1' and 'b2') may be consumed for immediate operands; if
the instruction is shorter, the trailing bytes are ignored.
-}
decode :: Word8 -> Word8 -> Word8 -> Decoded
decode b0 b1 b2 = case b0 of
    0x00 -> dec1 Nop
    0x76 -> dec1 Halt
    0xC9 -> dec1 Ret
    0xC3 -> dec3 (JpNN (combine b1 b2))
    0xCD -> dec3 (CallNN (combine b1 b2))
    -- LD rr, d16: 0x?1 with rr in bits 4..5
    0x01 -> dec3 (LdRrD16 RBC (combine b1 b2))
    0x11 -> dec3 (LdRrD16 RDE (combine b1 b2))
    0x21 -> dec3 (LdRrD16 RHL (combine b1 b2))
    0x31 -> dec3 (LdRrD16 RSP (combine b1 b2))
    -- INC rr / DEC rr: 0x?3 / 0x?B
    0x03 -> dec1 (IncRr RBC)
    0x13 -> dec1 (IncRr RDE)
    0x23 -> dec1 (IncRr RHL)
    0x33 -> dec1 (IncRr RSP)
    0x0B -> dec1 (DecRr RBC)
    0x1B -> dec1 (DecRr RDE)
    0x2B -> dec1 (DecRr RHL)
    0x3B -> dec1 (DecRr RSP)
    -- LD A,(BC)/(DE) and LD (BC)/(DE),A
    0x02 -> dec1 LdBCA
    0x12 -> dec1 LdDEA
    0x0A -> dec1 LdABC
    0x1A -> dec1 LdADE
    -- LD (HL+/-),A and LD A,(HL+/-)
    0x22 -> dec1 LdHLIA
    0x32 -> dec1 LdHLDA
    0x2A -> dec1 LdAHLI
    0x3A -> dec1 LdAHLD
    -- JR
    0x18 -> dec2 (Jr (signedByte b1))
    0x20 -> dec2 (JrCC CondNZ (signedByte b1))
    0x28 -> dec2 (JrCC CondZ (signedByte b1))
    0x30 -> dec2 (JrCC CondNC (signedByte b1))
    0x38 -> dec2 (JrCC CondC (signedByte b1))
    -- PUSH/POP rr
    0xC5 -> dec1 (PushRr SBC)
    0xD5 -> dec1 (PushRr SDE)
    0xE5 -> dec1 (PushRr SHL)
    0xF5 -> dec1 (PushRr SAF)
    0xC1 -> dec1 (PopRr SBC)
    0xD1 -> dec1 (PopRr SDE)
    0xE1 -> dec1 (PopRr SHL)
    0xF1 -> dec1 (PopRr SAF)
    -- Regular blocks decoded by bit pattern.
    op
        -- LD r, d8: 00_xxx_110
        | (op .&. 0xC7) == 0x06 ->
            dec2 (LdRD8 (decReg8 ((op `shiftR` 3) .&. 0x07)) b1)
        -- INC r: 00_xxx_100
        | (op .&. 0xC7) == 0x04 ->
            dec1 (IncR (decReg8 ((op `shiftR` 3) .&. 0x07)))
        -- DEC r: 00_xxx_101
        | (op .&. 0xC7) == 0x05 ->
            dec1 (DecR (decReg8 ((op `shiftR` 3) .&. 0x07)))
        -- LD r, r': 01_xxx_yyy (0x76 already handled above as HALT).
        | (op .&. 0xC0) == 0x40 ->
            let !dst = decReg8 ((op `shiftR` 3) .&. 0x07)
                !src = decReg8 (op .&. 0x07)
             in dec1 (LdRR dst src)
        -- ALU r: 10_xxx_yyy
        | (op .&. 0xC0) == 0x80 ->
            let !alu = decAluOp ((op `shiftR` 3) .&. 0x07)
                !src = decReg8 (op .&. 0x07)
             in dec1 (AluR alu src)
        -- ALU d8: 11_xxx_110
        | (op .&. 0xC7) == 0xC6 ->
            let !alu = decAluOp ((op `shiftR` 3) .&. 0x07)
             in dec2 (AluD8 alu b1)
        | otherwise -> dec1 (Unknown op)
  where
    dec1 i = Decoded i 1
    dec2 i = Decoded i 2
    dec3 i = Decoded i 3

combine :: Word8 -> Word8 -> Word16
combine lo hi = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

signedByte :: Word8 -> Int8
signedByte = fromIntegral

decReg8 :: Word8 -> Reg8
decReg8 n = toEnum (fromIntegral n)

decAluOp :: Word8 -> AluOp
decAluOp n = toEnum (fromIntegral n)
