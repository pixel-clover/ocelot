{-# LANGUAGE BangPatterns #-}

{- | SM83 fetch / decode / execute step.

'step' reads the byte at @PC@ (and the next two bytes for immediates), hands
them to 'Ocelot.Cpu.Decode.decode', advances @PC@ past the instruction, then
dispatches on the resulting 'Instruction' to mutate the 'Machine'. Cycles
spent on each instruction are folded into 'cpuCycles'.

'Unknown' opcodes set 'cpuHalted' so the run loop stops at the first opcode
that isn't yet implemented.
-}
module Ocelot.Cpu.Execute (
    MCycles,
    step,
    runUntilHalt,
    flagsToByte,
    byteToFlags,
) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Int (Int8)
import Data.Word (Word16, Word8)
import qualified Ocelot.Cpu.Alu as Alu
import Ocelot.Cpu.Decode (
    AluOp (..),
    Cond (..),
    Decoded (..),
    Instruction (..),
    Reg16 (..),
    Reg16Stack (..),
    Reg8 (..),
    decode,
 )
import Ocelot.Cpu.Registers (
    getAF,
    getBC,
    getDE,
    getHL,
    regA,
    regB,
    regC,
    regD,
    regE,
    regF,
    regH,
    regL,
    regPC,
    regSP,
    setAF,
    setBC,
    setDE,
    setHL,
 )
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (
    Machine (..),
    getCpuRegs,
    mapCpu,
    mapCpuRegs,
    readMem,
    writeMem,
 )

type MCycles = Int

-- | Run a single instruction. If the CPU is halted, 'step' is a no-op.
step :: Machine -> Machine
step m
    | cpuHalted (machineCpu m) = m
    | otherwise =
        let !pc = regPC (getCpuRegs m)
            !b0 = readMem pc m
            !b1 = readMem (pc + 1) m
            !b2 = readMem (pc + 2) m
            Decoded instr len = decode b0 b1 b2
            !m1 = mapCpuRegs (\r -> r{regPC = pc + fromIntegral len}) m
            (!m2, !mc) = execute instr m1
         in mapCpu (\c -> c{cpuCycles = cpuCycles c + fromIntegral mc}) m2

{- | Repeatedly call 'step' until the CPU halts, with a hard cap on the
number of iterations to keep accidental infinite loops out of the test
suite. Returns the final 'Machine' and the number of steps actually taken.
-}
runUntilHalt :: Int -> Machine -> (Machine, Int)
runUntilHalt cap = go 0
  where
    go !n !m
        | n >= cap = (m, n)
        | cpuHalted (machineCpu m) = (m, n)
        | otherwise = go (n + 1) (step m)

execute :: Instruction -> Machine -> (Machine, MCycles)
execute instr m = case instr of
    Nop -> (m, 1)
    Halt -> (mapCpu (\c -> c{cpuHalted = True}) m, 1)
    LdRR dst src ->
        let v = getReg8 src m
         in (setReg8 dst v m, ldRRcycles dst src)
    LdRD8 dst v -> (setReg8 dst v m, if dst == RIndHL then 3 else 2)
    LdRrD16 rr v -> (setReg16 rr v m, 3)
    LdABC -> (setReg8 RA (readMem (getReg16 RBC m) m) m, 2)
    LdADE -> (setReg8 RA (readMem (getReg16 RDE m) m) m, 2)
    LdBCA -> (writeMem (getReg16 RBC m) (getReg8 RA m) m, 2)
    LdDEA -> (writeMem (getReg16 RDE m) (getReg8 RA m) m, 2)
    LdAHLI ->
        let !hl = getReg16 RHL m
            !m1 = setReg8 RA (readMem hl m) m
         in (setReg16 RHL (hl + 1) m1, 2)
    LdAHLD ->
        let !hl = getReg16 RHL m
            !m1 = setReg8 RA (readMem hl m) m
         in (setReg16 RHL (hl - 1) m1, 2)
    LdHLIA ->
        let !hl = getReg16 RHL m
            !m1 = writeMem hl (getReg8 RA m) m
         in (setReg16 RHL (hl + 1) m1, 2)
    LdHLDA ->
        let !hl = getReg16 RHL m
            !m1 = writeMem hl (getReg8 RA m) m
         in (setReg16 RHL (hl - 1) m1, 2)
    IncR r -> applyInc r m
    DecR r -> applyDec r m
    IncRr rr -> (setReg16 rr (getReg16 rr m + 1) m, 2)
    DecRr rr -> (setReg16 rr (getReg16 rr m - 1) m, 2)
    AluR op r ->
        ( applyAlu op (getReg8 r m) m
        , if r == RIndHL then 2 else 1
        )
    AluD8 op v -> (applyAlu op v m, 2)
    Jr e -> (jumpRelative e m, 3)
    JrCC c e ->
        if testCond c m
            then (jumpRelative e m, 3)
            else (m, 2)
    JpNN nn -> (mapCpuRegs (\r -> r{regPC = nn}) m, 4)
    CallNN nn ->
        let !pc = regPC (getCpuRegs m)
            !m1 = pushWord pc m
         in (mapCpuRegs (\r -> r{regPC = nn}) m1, 6)
    Ret ->
        let (target, m1) = popWord m
         in (mapCpuRegs (\r -> r{regPC = target}) m1, 4)
    PushRr s -> (pushWord (getReg16Stack s m) m, 4)
    PopRr s ->
        let (v, m') = popWord m
         in (setReg16Stack s v m', 3)
    Unknown _ -> (mapCpu (\c -> c{cpuHalted = True}) m, 1)

ldRRcycles :: Reg8 -> Reg8 -> MCycles
ldRRcycles RIndHL _ = 2
ldRRcycles _ RIndHL = 2
ldRRcycles _ _ = 1

applyInc :: Reg8 -> Machine -> (Machine, MCycles)
applyInc r m =
    let v = getReg8 r m
        cIn = getFlagC m
        (v', flags) = Alu.inc8 v cIn
        m1 = setReg8 r v' m
     in (setFlagsByte (flagsToByte flags) m1, if r == RIndHL then 3 else 1)

applyDec :: Reg8 -> Machine -> (Machine, MCycles)
applyDec r m =
    let v = getReg8 r m
        cIn = getFlagC m
        (v', flags) = Alu.dec8 v cIn
        m1 = setReg8 r v' m
     in (setFlagsByte (flagsToByte flags) m1, if r == RIndHL then 3 else 1)

applyAlu :: AluOp -> Word8 -> Machine -> Machine
applyAlu op operand m =
    let a = getReg8 RA m
        cIn = getFlagC m
        (resultMaybe, flags) = case op of
            AluAdd -> let (r, f) = Alu.add8 a operand in (Just r, f)
            AluAdc -> let (r, f) = Alu.adc8 a operand cIn in (Just r, f)
            AluSub -> let (r, f) = Alu.sub8 a operand in (Just r, f)
            AluSbc -> let (r, f) = Alu.sbc8 a operand cIn in (Just r, f)
            AluAnd -> let (r, f) = Alu.and8 a operand in (Just r, f)
            AluXor -> let (r, f) = Alu.xor8 a operand in (Just r, f)
            AluOr -> let (r, f) = Alu.or8 a operand in (Just r, f)
            AluCp -> (Nothing, Alu.cp8 a operand)
        m1 = case resultMaybe of
            Just r -> setReg8 RA r m
            Nothing -> m
     in setFlagsByte (flagsToByte flags) m1

jumpRelative :: Int8 -> Machine -> Machine
jumpRelative e m =
    let !pc = regPC (getCpuRegs m)
        !target = pc + fromIntegral e
     in mapCpuRegs (\r -> r{regPC = target}) m

testCond :: Cond -> Machine -> Bool
testCond c m =
    let f = regF (getCpuRegs m)
     in case c of
            CondNZ -> not (testBit f 7)
            CondZ -> testBit f 7
            CondNC -> not (testBit f 4)
            CondC -> testBit f 4

pushWord :: Word16 -> Machine -> Machine
pushWord v m =
    let !sp = regSP (getCpuRegs m)
        !hi = fromIntegral (v `shiftR` 8) :: Word8
        !lo = fromIntegral (v .&. 0xFF) :: Word8
        !sp1 = sp - 1
        !m1 = writeMem sp1 hi m
        !sp2 = sp1 - 1
        !m2 = writeMem sp2 lo m1
     in mapCpuRegs (\r -> r{regSP = sp2}) m2

popWord :: Machine -> (Word16, Machine)
popWord m =
    let !sp = regSP (getCpuRegs m)
        !lo = readMem sp m
        !hi = readMem (sp + 1) m
        !v = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
     in (v, mapCpuRegs (\r -> r{regSP = sp + 2}) m)

getReg8 :: Reg8 -> Machine -> Word8
getReg8 r m = case r of
    RA -> regA (getCpuRegs m)
    RB -> regB (getCpuRegs m)
    RC -> regC (getCpuRegs m)
    RD -> regD (getCpuRegs m)
    RE -> regE (getCpuRegs m)
    RH -> regH (getCpuRegs m)
    RL -> regL (getCpuRegs m)
    RIndHL -> readMem (getReg16 RHL m) m

setReg8 :: Reg8 -> Word8 -> Machine -> Machine
setReg8 r v m = case r of
    RA -> mapCpuRegs (\rs -> rs{regA = v}) m
    RB -> mapCpuRegs (\rs -> rs{regB = v}) m
    RC -> mapCpuRegs (\rs -> rs{regC = v}) m
    RD -> mapCpuRegs (\rs -> rs{regD = v}) m
    RE -> mapCpuRegs (\rs -> rs{regE = v}) m
    RH -> mapCpuRegs (\rs -> rs{regH = v}) m
    RL -> mapCpuRegs (\rs -> rs{regL = v}) m
    RIndHL -> writeMem (getReg16 RHL m) v m

getReg16 :: Reg16 -> Machine -> Word16
getReg16 r m = case r of
    RBC -> getBC (getCpuRegs m)
    RDE -> getDE (getCpuRegs m)
    RHL -> getHL (getCpuRegs m)
    RSP -> regSP (getCpuRegs m)

setReg16 :: Reg16 -> Word16 -> Machine -> Machine
setReg16 r v = case r of
    RBC -> mapCpuRegs (setBC v)
    RDE -> mapCpuRegs (setDE v)
    RHL -> mapCpuRegs (setHL v)
    RSP -> mapCpuRegs (\rs -> rs{regSP = v})

getReg16Stack :: Reg16Stack -> Machine -> Word16
getReg16Stack s m = case s of
    SAF -> getAF (getCpuRegs m)
    SBC -> getBC (getCpuRegs m)
    SDE -> getDE (getCpuRegs m)
    SHL -> getHL (getCpuRegs m)

setReg16Stack :: Reg16Stack -> Word16 -> Machine -> Machine
setReg16Stack s v = case s of
    SAF -> mapCpuRegs (setAF v)
    SBC -> mapCpuRegs (setBC v)
    SDE -> mapCpuRegs (setDE v)
    SHL -> mapCpuRegs (setHL v)

flagsToByte :: Alu.Flags -> Word8
flagsToByte (Alu.Flags z n h c) =
    (if z then 0x80 else 0)
        .|. (if n then 0x40 else 0)
        .|. (if h then 0x20 else 0)
        .|. (if c then 0x10 else 0)

byteToFlags :: Word8 -> Alu.Flags
byteToFlags b =
    Alu.Flags (testBit b 7) (testBit b 6) (testBit b 5) (testBit b 4)

setFlagsByte :: Word8 -> Machine -> Machine
setFlagsByte b = mapCpuRegs (\r -> r{regF = b .&. 0xF0})

getFlagC :: Machine -> Bool
getFlagC m = testBit (regF (getCpuRegs m)) 4
