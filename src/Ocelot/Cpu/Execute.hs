{-# LANGUAGE BangPatterns #-}

{- | SM83 fetch / decode / execute step.

@step@ reads the byte at @PC@ (and the next two bytes for immediates), hands
them to 'Ocelot.Cpu.Decode.decode', advances @PC@ past the instruction, then
dispatches on the 'Instruction'. Cycles spent on each instruction are folded
into 'cpuCycles' and the bus is advanced so subsystems (Timer, PPU) tick.

Step semantics:

1. If the EI delay is active, set @IME@, then run one instruction. Interrupts
   are not serviced this step.
2. Else, if @IME@ is set and an interrupt is pending in @IF & IE@, service
   it (push @PC@, clear the @IF@ bit, clear @IME@, jump to vector,
   5 M-cycles). No instruction is fetched this step.
3. Else, if the CPU is halted: tick the bus by 1 M-cycle. If an interrupt
   becomes pending the next step either services it (when @IME@ is set) or
   simply wakes (when @IME@ is clear).
4. Otherwise fetch / decode / execute one instruction.

@Unknown@ opcodes set 'cpuHalted' so a run loop stops at the first opcode
that isn't yet implemented.
-}
module Ocelot.Cpu.Execute (
    MCycles,
    step,
    runUntilHalt,
    runFor,
    flagsToByte,
    byteToFlags,
    pendingInterrupt,
    interruptVector,
) where

import Data.Bits (clearBit, complement, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Int (Int8)
import Data.Word (Word16, Word8)
import qualified Ocelot.Bus as Bus
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
    advanceBus,
    getCpu,
    getCpuRegs,
    mapCpu,
    mapCpuRegs,
    readMem,
    writeMem,
 )

type MCycles = Int

step :: Machine -> IO ()
step m = do
    cpu <- getCpu m
    if cpuEiDelay cpu
        then do
            mapCpu (\c -> c{cpuIme = True, cpuEiDelay = False}) m
            cpu' <- getCpu m
            if cpuHalted cpu'
                then haltStep m
                else doInstruction m
        else
            if cpuIme cpu
                then do
                    mIrq <- pendingInterrupt m
                    case mIrq of
                        Just irq -> serviceInterrupt irq m
                        Nothing ->
                            if cpuHalted cpu
                                then haltStep m
                                else doInstruction m
                else
                    if cpuHalted cpu
                        then haltStep m
                        else doInstruction m

haltStep :: Machine -> IO ()
haltStep m = do
    mIrq <- pendingInterrupt m
    case mIrq of
        Just _ -> do
            mapCpu (\c -> c{cpuHalted = False, cpuCycles = cpuCycles c + 1}) m
            advanceBus 1 m
        Nothing -> do
            mapCpu (\c -> c{cpuCycles = cpuCycles c + 1}) m
            advanceBus 1 m

doInstruction :: Machine -> IO ()
doInstruction m = do
    pc <- regPC <$> getCpuRegs m
    b0 <- readMem pc m
    b1 <- readMem (pc + 1) m
    b2 <- readMem (pc + 2) m
    let Decoded instr len = decode b0 b1 b2
    mapCpuRegs (\r -> r{regPC = pc + fromIntegral len}) m
    mc <- execute instr m
    mapCpu (\c -> c{cpuCycles = cpuCycles c + fromIntegral mc}) m
    advanceBus mc m

pendingInterrupt :: Machine -> IO (Maybe Int)
pendingInterrupt m = do
    iflag <- readMem 0xFF0F m
    ie <- readMem 0xFFFF m
    let active = iflag .&. ie .&. 0x1F
    pure $ if active == 0 then Nothing else Just (lowestSetBit active)

interruptVector :: Int -> Word16
interruptVector n = 0x40 + fromIntegral n * 8

lowestSetBit :: Word8 -> Int
lowestSetBit = go 0
  where
    go i w
        | i >= 5 = 4
        | testBit w i = i
        | otherwise = go (i + 1) w

serviceInterrupt :: Int -> Machine -> IO ()
serviceInterrupt n m = do
    iflag <- readMem 0xFF0F m
    writeMem 0xFF0F (clearBit iflag n) m
    mapCpu (\c -> c{cpuIme = False, cpuHalted = False}) m
    pc <- regPC <$> getCpuRegs m
    pushWord pc m
    mapCpuRegs (\r -> r{regPC = interruptVector n}) m
    mapCpu (\c -> c{cpuCycles = cpuCycles c + 5}) m
    advanceBus 5 m

runUntilHalt :: Int -> Machine -> IO Int
runUntilHalt cap = go 0
  where
    go !n !m
        | n >= cap = pure n
        | otherwise = do
            cpu <- getCpu m
            if cpuHalted cpu
                then pure n
                else step m >> go (n + 1) m

runFor :: Int -> Machine -> IO Int
runFor cap = go 0
  where
    go !n !m
        | n >= cap = pure n
        | otherwise = step m >> go (n + 1) m

----------------------------------------------------------------------
-- Per-instruction handlers
----------------------------------------------------------------------

execute :: Instruction -> Machine -> IO MCycles
execute instr m = case instr of
    Nop -> pure 1
    Halt -> do
        -- HALT bug: when IME=0 and at least one pending interrupt is enabled
        -- (IF & IE != 0), the CPU does not halt. Real hardware also fails to
        -- advance PC on the next fetch (executing the following instruction
        -- twice); we model the simpler "skip the halt" form, which is what
        -- most ROMs need to avoid stalling.
        cpu <- getCpu m
        if not (cpuIme cpu)
            then do
                iflag <- Bus.read8 0xFF0F (machineBus m)
                ie <- Bus.read8 0xFFFF (machineBus m)
                if (iflag .&. ie .&. 0x1F) /= 0
                    then pure 1
                    else mapCpu (\c -> c{cpuHalted = True}) m >> pure 1
            else mapCpu (\c -> c{cpuHalted = True}) m >> pure 1
    LdRR dst src -> do
        v <- getReg8 src m
        setReg8 dst v m
        pure (ldRRcycles dst src)
    LdRD8 dst v -> setReg8 dst v m >> pure (if dst == RIndHL then 3 else 2)
    LdRrD16 rr v -> setReg16 rr v m >> pure 3
    LdABC -> do
        a <- readMem <$> getReg16 RBC m <*> pure m
        a' <- a
        setReg8 RA a' m
        pure 2
    LdADE -> do
        addr <- getReg16 RDE m
        v <- readMem addr m
        setReg8 RA v m
        pure 2
    LdBCA -> do
        addr <- getReg16 RBC m
        a <- getReg8 RA m
        writeMem addr a m
        pure 2
    LdDEA -> do
        addr <- getReg16 RDE m
        a <- getReg8 RA m
        writeMem addr a m
        pure 2
    LdAHLI -> do
        hl <- getReg16 RHL m
        v <- readMem hl m
        setReg8 RA v m
        setReg16 RHL (hl + 1) m
        pure 2
    LdAHLD -> do
        hl <- getReg16 RHL m
        v <- readMem hl m
        setReg8 RA v m
        setReg16 RHL (hl - 1) m
        pure 2
    LdHLIA -> do
        hl <- getReg16 RHL m
        a <- getReg8 RA m
        writeMem hl a m
        setReg16 RHL (hl + 1) m
        pure 2
    LdHLDA -> do
        hl <- getReg16 RHL m
        a <- getReg8 RA m
        writeMem hl a m
        setReg16 RHL (hl - 1) m
        pure 2
    IncR r -> applyInc r m
    DecR r -> applyDec r m
    IncRr rr -> do
        v <- getReg16 rr m
        setReg16 rr (v + 1) m
        pure 2
    DecRr rr -> do
        v <- getReg16 rr m
        setReg16 rr (v - 1) m
        pure 2
    AluR op r -> do
        v <- getReg8 r m
        applyAlu op v m
        pure (if r == RIndHL then 2 else 1)
    AluD8 op v -> applyAlu op v m >> pure 2
    Jr e -> jumpRelative e m >> pure 3
    JrCC c e -> do
        b <- testCond c m
        if b then jumpRelative e m >> pure 3 else pure 2
    JpNN nn -> mapCpuRegs (\r -> r{regPC = nn}) m >> pure 4
    CallNN nn -> do
        pc <- regPC <$> getCpuRegs m
        pushWord pc m
        mapCpuRegs (\r -> r{regPC = nn}) m
        pure 6
    Ret -> do
        target <- popWord m
        mapCpuRegs (\r -> r{regPC = target}) m
        pure 4
    PushRr s -> do
        v <- getReg16Stack s m
        pushWord v m
        pure 4
    PopRr s -> do
        v <- popWord m
        setReg16Stack s v m
        pure 3
    JpCC c nn -> do
        b <- testCond c m
        if b
            then mapCpuRegs (\r -> r{regPC = nn}) m >> pure 4
            else pure 3
    CallCC c nn -> do
        b <- testCond c m
        if b
            then do
                pc <- regPC <$> getCpuRegs m
                pushWord pc m
                mapCpuRegs (\r -> r{regPC = nn}) m
                pure 6
            else pure 3
    RetCC c -> do
        b <- testCond c m
        if b
            then do
                target <- popWord m
                mapCpuRegs (\r -> r{regPC = target}) m
                pure 5
            else pure 2
    Reti -> do
        target <- popWord m
        mapCpuRegs (\r -> r{regPC = target}) m
        mapCpu (\c -> c{cpuIme = True}) m
        pure 4
    JpHL -> do
        hl <- getReg16 RHL m
        mapCpuRegs (\r -> r{regPC = hl}) m
        pure 1
    Rst target -> do
        pc <- regPC <$> getCpuRegs m
        pushWord pc m
        mapCpuRegs (\r -> r{regPC = fromIntegral target}) m
        pure 4
    AddHlRr rr -> do
        hl <- getReg16 RHL m
        v <- getReg16 rr m
        zIn <- (`testBit` 7) . regF <$> getCpuRegs m
        let (r, flags) = Alu.add16 hl v zIn
        setReg16 RHL r m
        setFlagsByte (flagsToByte flags) m
        pure 2
    AddSpE e -> do
        sp <- regSP <$> getCpuRegs m
        let (r, flags) = Alu.addSP sp e
        mapCpuRegs (\rs -> rs{regSP = r}) m
        setFlagsByte (flagsToByte flags) m
        pure 4
    LdHlSpE e -> do
        sp <- regSP <$> getCpuRegs m
        let (r, flags) = Alu.addSP sp e
        setReg16 RHL r m
        setFlagsByte (flagsToByte flags) m
        pure 3
    LdSpHl -> do
        hl <- getReg16 RHL m
        mapCpuRegs (\r -> r{regSP = hl}) m
        pure 2
    LdhNA n -> do
        a <- getReg8 RA m
        writeMem (0xFF00 + fromIntegral n) a m
        pure 3
    LdhAN n -> do
        v <- readMem (0xFF00 + fromIntegral n) m
        setReg8 RA v m
        pure 3
    LdCA -> do
        cv <- regC <$> getCpuRegs m
        a <- getReg8 RA m
        writeMem (0xFF00 + fromIntegral cv) a m
        pure 2
    LdAC -> do
        cv <- regC <$> getCpuRegs m
        v <- readMem (0xFF00 + fromIntegral cv) m
        setReg8 RA v m
        pure 2
    LdNNA nn -> do
        a <- getReg8 RA m
        writeMem nn a m
        pure 4
    LdANN nn -> do
        v <- readMem nn m
        setReg8 RA v m
        pure 4
    LdNNSp nn -> do
        sp <- regSP <$> getCpuRegs m
        let lo = fromIntegral (sp .&. 0xFF) :: Word8
            hi = fromIntegral (sp `shiftR` 8) :: Word8
        writeMem nn lo m
        writeMem (nn + 1) hi m
        pure 5
    Rlca -> aRotate Alu.rlc8 m
    Rrca -> aRotate Alu.rrc8 m
    Rla -> aRotateC Alu.rl8 m
    Rra -> aRotateC Alu.rr8 m
    Daa -> do
        regs <- getCpuRegs m
        let a = regA regs
            n = testBit (regF regs) 6
            h = testBit (regF regs) 5
            c = testBit (regF regs) 4
            (a', flags) = Alu.daa a n h c
        setReg8 RA a' m
        setFlagsByte (flagsToByte flags) m
        pure 1
    Cpl -> do
        a <- getReg8 RA m
        regs <- getCpuRegs m
        let zF = testBit (regF regs) 7
            cF = testBit (regF regs) 4
            flags = Alu.Flags zF True True cF
        setReg8 RA (complement a) m
        setFlagsByte (flagsToByte flags) m
        pure 1
    Scf -> do
        zF <- (\r -> testBit (regF r) 7) <$> getCpuRegs m
        setFlagsByte (flagsToByte (Alu.Flags zF False False True)) m
        pure 1
    Ccf -> do
        regs <- getCpuRegs m
        let zF = testBit (regF regs) 7
            cF = testBit (regF regs) 4
        setFlagsByte (flagsToByte (Alu.Flags zF False False (not cF))) m
        pure 1
    Di -> mapCpu (\c -> c{cpuIme = False, cpuEiDelay = False}) m >> pure 1
    Ei -> mapCpu (\c -> c{cpuEiDelay = True}) m >> pure 1
    Stop -> do
        -- On a CGB cart with KEY1 bit 0 set, STOP triggers the
        -- single/double-speed switch instead of halting; otherwise it
        -- halts as on DMG.
        switched <- Bus.triggerSpeedSwitch (machineBus m)
        if switched
            then pure 1
            else mapCpu (\c -> c{cpuHalted = True}) m >> pure 1
    Rlc r -> cbRotate Alu.rlc8 r m
    Rrc r -> cbRotate Alu.rrc8 r m
    Rl r -> cbRotateC Alu.rl8 r m
    Rr r -> cbRotateC Alu.rr8 r m
    Sla r -> cbRotate Alu.sla8 r m
    Sra r -> cbRotate Alu.sra8 r m
    Swap r -> cbRotate Alu.swap8 r m
    Srl r -> cbRotate Alu.srl8 r m
    BitOp b r -> do
        v <- getReg8 r m
        cIn <- getFlagC m
        let flags = Alu.bit8 b v cIn
        setFlagsByte (flagsToByte flags) m
        pure (if r == RIndHL then 3 else 2)
    Res b r -> do
        v <- getReg8 r m
        setReg8 r (clearBit v b) m
        pure (if r == RIndHL then 4 else 2)
    Set b r -> do
        v <- getReg8 r m
        setReg8 r (setBit v b) m
        pure (if r == RIndHL then 4 else 2)
    Unknown _ -> mapCpu (\c -> c{cpuHalted = True}) m >> pure 1

ldRRcycles :: Reg8 -> Reg8 -> MCycles
ldRRcycles RIndHL _ = 2
ldRRcycles _ RIndHL = 2
ldRRcycles _ _ = 1

applyInc :: Reg8 -> Machine -> IO MCycles
applyInc r m = do
    v <- getReg8 r m
    cIn <- getFlagC m
    let (v', flags) = Alu.inc8 v cIn
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 3 else 1)

applyDec :: Reg8 -> Machine -> IO MCycles
applyDec r m = do
    v <- getReg8 r m
    cIn <- getFlagC m
    let (v', flags) = Alu.dec8 v cIn
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 3 else 1)

applyAlu :: AluOp -> Word8 -> Machine -> IO ()
applyAlu op operand m = do
    a <- getReg8 RA m
    cIn <- getFlagC m
    let (resultMaybe, flags) = case op of
            AluAdd -> let (r, f) = Alu.add8 a operand in (Just r, f)
            AluAdc -> let (r, f) = Alu.adc8 a operand cIn in (Just r, f)
            AluSub -> let (r, f) = Alu.sub8 a operand in (Just r, f)
            AluSbc -> let (r, f) = Alu.sbc8 a operand cIn in (Just r, f)
            AluAnd -> let (r, f) = Alu.and8 a operand in (Just r, f)
            AluXor -> let (r, f) = Alu.xor8 a operand in (Just r, f)
            AluOr -> let (r, f) = Alu.or8 a operand in (Just r, f)
            AluCp -> (Nothing, Alu.cp8 a operand)
    case resultMaybe of
        Just r -> setReg8 RA r m
        Nothing -> pure ()
    setFlagsByte (flagsToByte flags) m

aRotate :: (Word8 -> (Word8, Alu.Flags)) -> Machine -> IO MCycles
aRotate op m = do
    a <- getReg8 RA m
    let (a', flags) = op a
        flags' = flags{Alu.flagZ = False}
    setReg8 RA a' m
    setFlagsByte (flagsToByte flags') m
    pure 1

aRotateC :: (Word8 -> Bool -> (Word8, Alu.Flags)) -> Machine -> IO MCycles
aRotateC op m = do
    a <- getReg8 RA m
    cIn <- getFlagC m
    let (a', flags) = op a cIn
        flags' = flags{Alu.flagZ = False}
    setReg8 RA a' m
    setFlagsByte (flagsToByte flags') m
    pure 1

cbRotate ::
    (Word8 -> (Word8, Alu.Flags)) ->
    Reg8 ->
    Machine ->
    IO MCycles
cbRotate op r m = do
    v <- getReg8 r m
    let (v', flags) = op v
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 4 else 2)

cbRotateC ::
    (Word8 -> Bool -> (Word8, Alu.Flags)) ->
    Reg8 ->
    Machine ->
    IO MCycles
cbRotateC op r m = do
    v <- getReg8 r m
    cIn <- getFlagC m
    let (v', flags) = op v cIn
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 4 else 2)

jumpRelative :: Int8 -> Machine -> IO ()
jumpRelative e m = do
    pc <- regPC <$> getCpuRegs m
    let target = pc + fromIntegral e
    mapCpuRegs (\r -> r{regPC = target}) m

testCond :: Cond -> Machine -> IO Bool
testCond c m = do
    f <- regF <$> getCpuRegs m
    pure $ case c of
        CondNZ -> not (testBit f 7)
        CondZ -> testBit f 7
        CondNC -> not (testBit f 4)
        CondC -> testBit f 4

pushWord :: Word16 -> Machine -> IO ()
pushWord v m = do
    sp <- regSP <$> getCpuRegs m
    let hi = fromIntegral (v `shiftR` 8) :: Word8
        lo = fromIntegral (v .&. 0xFF) :: Word8
        sp1 = sp - 1
        sp2 = sp1 - 1
    writeMem sp1 hi m
    writeMem sp2 lo m
    mapCpuRegs (\r -> r{regSP = sp2}) m

popWord :: Machine -> IO Word16
popWord m = do
    sp <- regSP <$> getCpuRegs m
    lo <- readMem sp m
    hi <- readMem (sp + 1) m
    let v = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
    mapCpuRegs (\r -> r{regSP = sp + 2}) m
    pure v

getReg8 :: Reg8 -> Machine -> IO Word8
getReg8 r m = case r of
    RA -> regA <$> getCpuRegs m
    RB -> regB <$> getCpuRegs m
    RC -> regC <$> getCpuRegs m
    RD -> regD <$> getCpuRegs m
    RE -> regE <$> getCpuRegs m
    RH -> regH <$> getCpuRegs m
    RL -> regL <$> getCpuRegs m
    RIndHL -> do
        hl <- getReg16 RHL m
        readMem hl m

setReg8 :: Reg8 -> Word8 -> Machine -> IO ()
setReg8 r v m = case r of
    RA -> mapCpuRegs (\rs -> rs{regA = v}) m
    RB -> mapCpuRegs (\rs -> rs{regB = v}) m
    RC -> mapCpuRegs (\rs -> rs{regC = v}) m
    RD -> mapCpuRegs (\rs -> rs{regD = v}) m
    RE -> mapCpuRegs (\rs -> rs{regE = v}) m
    RH -> mapCpuRegs (\rs -> rs{regH = v}) m
    RL -> mapCpuRegs (\rs -> rs{regL = v}) m
    RIndHL -> do
        hl <- getReg16 RHL m
        writeMem hl v m

getReg16 :: Reg16 -> Machine -> IO Word16
getReg16 r m = case r of
    RBC -> getBC <$> getCpuRegs m
    RDE -> getDE <$> getCpuRegs m
    RHL -> getHL <$> getCpuRegs m
    RSP -> regSP <$> getCpuRegs m

setReg16 :: Reg16 -> Word16 -> Machine -> IO ()
setReg16 r v m = case r of
    RBC -> mapCpuRegs (setBC v) m
    RDE -> mapCpuRegs (setDE v) m
    RHL -> mapCpuRegs (setHL v) m
    RSP -> mapCpuRegs (\rs -> rs{regSP = v}) m

getReg16Stack :: Reg16Stack -> Machine -> IO Word16
getReg16Stack s m = case s of
    SAF -> getAF <$> getCpuRegs m
    SBC -> getBC <$> getCpuRegs m
    SDE -> getDE <$> getCpuRegs m
    SHL -> getHL <$> getCpuRegs m

setReg16Stack :: Reg16Stack -> Word16 -> Machine -> IO ()
setReg16Stack s v m = case s of
    SAF -> mapCpuRegs (setAF v) m
    SBC -> mapCpuRegs (setBC v) m
    SDE -> mapCpuRegs (setDE v) m
    SHL -> mapCpuRegs (setHL v) m

flagsToByte :: Alu.Flags -> Word8
flagsToByte (Alu.Flags z n h c) =
    (if z then 0x80 else 0)
        .|. (if n then 0x40 else 0)
        .|. (if h then 0x20 else 0)
        .|. (if c then 0x10 else 0)

byteToFlags :: Word8 -> Alu.Flags
byteToFlags b =
    Alu.Flags (testBit b 7) (testBit b 6) (testBit b 5) (testBit b 4)

setFlagsByte :: Word8 -> Machine -> IO ()
setFlagsByte b = mapCpuRegs (\r -> r{regF = b .&. 0xF0})

getFlagC :: Machine -> IO Bool
getFlagC m = (\r -> testBit (regF r) 4) <$> getCpuRegs m
