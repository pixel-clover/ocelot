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

import Control.Monad (forM_, when)
import Data.Bits (clearBit, complement, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.IORef (readIORef, writeIORef)
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
    cycleNoAccess,
    cycleRead,
    cycleWrite,
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
    cpu0 <- getCpu m
    let pc = regPC (cpuRegs cpu0)
    -- Reset the inline-advance counter BEFORE prefetch so that the
    -- M1/M2/M3 fetch ticks count toward the instruction's total
    -- inline advance, mirroring SameBoy's per-cycle 'cycle_read'
    -- model where the opcode and immediate fetches each cost
    -- 1 M-cycle of bus tick.
    writeIORef (machineInternalAdvance m) 0
    b0 <- cycleRead pc m
    -- Only fetch the immediate bytes the instruction actually
    -- consumes. The previous unconditional 3-byte fetch ticked extra
    -- cycles for short instructions and could touch memory-mapped
    -- registers at @pc+1@/@pc+2@.
    let len = opcodeLength b0
    -- HALT-bug fetch: SameBoy models the bug as a single @PC--@ after
    -- the opcode fetch, so subsequent operand fetches read from
    -- addresses one byte earlier than normal (the byte AT the opcode
    -- position becomes the first operand byte) and PC ends @len-1@
    -- forward instead of @len@. Without this offset our previous
    -- "don't advance PC, re-decode next iteration" model only matched
    -- hardware for 1-byte instructions; 2- and 3-byte instructions
    -- after HALT got the wrong operand bytes (off by +1) and the
    -- wrong final PC. Drives blargg @halt_bug.gb@ subtest 1.
    let !operandOffset = if cpuHaltBug cpu0 then 0 else 1
    b1 <- if len >= 2 then cycleRead (pc + operandOffset) m else pure 0
    b2 <- if len >= 3 then cycleRead (pc + operandOffset + 1) m else pure 0
    let Decoded instr _ = decode b0 b1 b2
    when (cpuHaltBug cpu0) $
        mapCpu (\c -> c{cpuHaltBug = False}) m
    mapCpuRegs (\r -> r{regPC = pc + fromIntegral len - (1 - operandOffset)}) m
    mc <- execute instr m
    mapCpu (\c -> c{cpuCycles = cpuCycles c + fromIntegral mc}) m
    consumed <- readIORef (machineInternalAdvance m)
    let remaining = mc - consumed
    when (remaining > 0) (advanceBus remaining m)

{- | Length in bytes of the instruction starting with opcode byte @b0@,
matching 'Ocelot.Cpu.Decode.decode'. Used by 'doInstruction' to avoid
reading immediate bytes the instruction does not consume.
-}
opcodeLength :: Word8 -> Int
opcodeLength b = case b of
    -- 3-byte (16-bit immediate or absolute address)
    0x01 -> 3
    0x08 -> 3
    0x11 -> 3
    0x21 -> 3
    0x31 -> 3
    0xC2 -> 3
    0xC3 -> 3
    0xC4 -> 3
    0xCA -> 3
    0xCC -> 3
    0xCD -> 3
    0xD2 -> 3
    0xD4 -> 3
    0xDA -> 3
    0xDC -> 3
    0xEA -> 3
    0xFA -> 3
    -- 2-byte (8-bit immediate, signed offset, or CB prefix)
    0x10 -> 2
    0x18 -> 2
    0x20 -> 2
    0x28 -> 2
    0x30 -> 2
    0x38 -> 2
    0xCB -> 2
    0xE0 -> 2
    0xE8 -> 2
    0xF0 -> 2
    0xF8 -> 2
    op
        | (op .&. 0xC7) == 0x06 -> 2 -- LD r, d8
        | (op .&. 0xC7) == 0xC6 -> 2 -- ALU A, d8
        | otherwise -> 1

pendingInterrupt :: Machine -> IO (Maybe Int)
pendingInterrupt m = do
    iflag <- readMem 0xFF0F m
    ie <- readMem 0xFFFF m
    let active = iflag .&. ie .&. 0x1F
    pure $ if active == 0 then Nothing else Just (lowestSetBit active)

interruptVector :: Int -> Word16
interruptVector n = 0x40 + fromIntegral n * 8

lowestSetBit :: Word8 -> Int
{-# INLINE lowestSetBit #-}
lowestSetBit = go 0
  where
    go i w
        | i >= 5 = 4
        | testBit w i = i
        | otherwise = go (i + 1) w

{- | Service a pending interrupt: 5 M-cycles, modeled cycle-accurately.

The sequence (per SameBoy 'sm83_cpu.c' interrupt service): 2 internal
cycles (a wasted opcode read + an OAM-bug-trigger cycle in real
hardware), then 1 internal cycle, then write PC hi at SP-1, then
write PC lo at SP-2. Each of the writes ticks the bus by 1 M-cycle
inline.

The interrupt vector is sampled BETWEEN M4 and M5 — after M4's write
has had any effect on @IF@/@IE@ (e.g. M4 lands on @0xFFFF@ when
@SP = 0x0000@, clobbering @IE@) but before M5's write. This
distinction is what mooneye 'interrupts/ie_push' verifies: with
@SP = 0x0001@ the M5 write lands on @IE@, but vector selection has
already been decided based on the original @IE@ value. With
@SP = 0x0000@ the M4 write to @IE@ is visible at sample time, so
the new value steers the vector. Sampling after M5 (as we did
previously) conflates the two scenarios.

The chosen @IF@ bit is cleared BEFORE M5 too, so a M5 write that
lands on @IF@ overwrites the cleared state with PC-lo (matching
SameBoy 'sm83_cpu.c' lines 1671-1697).
-}
serviceInterrupt :: Int -> Machine -> IO ()
serviceInterrupt _ m = do
    -- Reset the inline-advance counter so cycleRead/cycleWrite ticks
    -- here are tracked the same way as inside doInstruction.
    writeIORef (machineInternalAdvance m) 0
    cycleNoAccess m -- M1: wasted opcode read
    cycleNoAccess m -- M2: OAM-bug-trigger cycle
    pc <- regPC <$> getCpuRegs m
    sp <- regSP <$> getCpuRegs m
    let hi = fromIntegral (pc `shiftR` 8) :: Word8
        lo = fromIntegral (pc .&. 0xFF) :: Word8
    cycleNoAccess m -- M3: internal/SP--
    cycleWrite (sp - 1) hi m -- M4 (may land on IE if SP - 1 == 0xFFFF)
    -- Sample IF and IE BETWEEN M4 and M5. M4's write may have
    -- modified IE; M5's write hasn't happened yet.
    iflag <- readMem 0xFF0F m
    ie <- readMem 0xFFFF m
    let active = iflag .&. ie .&. 0x1F
        servicedBit = if active == 0 then Nothing else Just (lowestSetBit active)
        target = maybe 0x0000 interruptVector servicedBit
    -- Clear the chosen IF bit before M5. If M5 lands on IF
    -- (@SP - 2 == 0xFF0F@), the M5 write will overwrite this anyway.
    forM_ servicedBit (\b -> writeMem 0xFF0F (clearBit iflag b) m)
    cycleWrite (sp - 2) lo m -- M5 (may land on IE if SP - 2 == 0xFFFF, or on IF if SP - 2 == 0xFF0F)
    mapCpuRegs (\r -> r{regSP = sp - 2, regPC = target}) m
    mapCpu (\c -> c{cpuIme = False, cpuHalted = False, cpuCycles = cpuCycles c + 5}) m

runUntilHalt :: Int -> Machine -> IO Int
runUntilHalt cap = go 0
  where
    go !n !m
        | n >= cap = pure n
        | otherwise = do
            cpu <- getCpu m
            if cpuHalted cpu
                then pure n
                else do
                    let c0 = cpuCycles cpu
                    step m
                    c1 <- cpuCycles <$> getCpu m
                    go (n + fromIntegral (c1 - c0)) m

{- | Run the machine until at least @cap@ CPU M-cycles have been consumed.
Counting M-cycles (not instruction steps) ensures that the wall-clock
frame budget is respected regardless of the instruction mix: a CALL
(6 M-cycles) contributes 6 toward the cap, not 1.
-}
runFor :: Int -> Machine -> IO Int
runFor cap = go 0
  where
    go !n !m
        | n >= cap = pure n
        | otherwise = do
            c0 <- cpuCycles <$> getCpu m
            step m
            c1 <- cpuCycles <$> getCpu m
            go (n + fromIntegral (c1 - c0)) m

----------------------------------------------------------------------
-- Per-instruction handlers
----------------------------------------------------------------------

execute :: Instruction -> Machine -> IO MCycles
execute instr m = case instr of
    Nop -> pure 1
    Halt -> do
        -- HALT bug: when IME=0 and at least one pending interrupt is enabled
        -- (IF & IE != 0), the CPU does not halt and the next instruction
        -- fetch fails to advance PC, so the byte after HALT is decoded
        -- twice. We latch 'cpuHaltBug' here and consume it in the next
        -- 'doInstruction' call.
        cpu <- getCpu m
        if not (cpuIme cpu)
            then do
                iflag <- Bus.read8 0xFF0F (machineBus m)
                ie <- Bus.read8 0xFFFF (machineBus m)
                if (iflag .&. ie .&. 0x1F) /= 0
                    then mapCpu (\c -> c{cpuHaltBug = True}) m >> pure 1
                    else mapCpu (\c -> c{cpuHalted = True}) m >> pure 1
            else mapCpu (\c -> c{cpuHalted = True}) m >> pure 1
    LdRR dst src -> do
        v <- getReg8 src m
        setReg8 dst v m
        pure (ldRRcycles dst src)
    LdRD8 dst v -> setReg8 dst v m >> pure (if dst == RIndHL then 3 else 2)
    LdRrD16 rr v -> setReg16 rr v m >> pure 3
    LdABC -> do
        addr <- getReg16 RBC m
        v <- cycleRead addr m
        setReg8 RA v m
        pure 2
    LdADE -> do
        addr <- getReg16 RDE m
        v <- cycleRead addr m
        setReg8 RA v m
        pure 2
    LdBCA -> do
        addr <- getReg16 RBC m
        a <- getReg8 RA m
        cycleWrite addr a m
        pure 2
    LdDEA -> do
        addr <- getReg16 RDE m
        a <- getReg8 RA m
        cycleWrite addr a m
        pure 2
    LdAHLI -> do
        hl <- getReg16 RHL m
        v <- cycleRead hl m
        setReg8 RA v m
        setReg16 RHL (hl + 1) m
        pure 2
    LdAHLD -> do
        hl <- getReg16 RHL m
        v <- cycleRead hl m
        setReg8 RA v m
        setReg16 RHL (hl - 1) m
        pure 2
    LdHLIA -> do
        hl <- getReg16 RHL m
        a <- getReg8 RA m
        cycleWrite hl a m
        setReg16 RHL (hl + 1) m
        pure 2
    LdHLDA -> do
        hl <- getReg16 RHL m
        a <- getReg8 RA m
        cycleWrite hl a m
        setReg16 RHL (hl - 1) m
        pure 2
    IncR r -> applyInc r m
    DecR r -> applyDec r m
    -- INC rr / DEC rr are 2 M-cycles: M1 fetch + 1 internal cycle.
    IncRr rr -> do
        v <- getReg16 rr m
        setReg16 rr (v + 1) m
        cycleNoAccess m
        pure 2
    DecRr rr -> do
        v <- getReg16 rr m
        setReg16 rr (v - 1) m
        cycleNoAccess m
        pure 2
    AluR op r -> do
        v <- getReg8 r m
        applyAlu op v m
        pure (if r == RIndHL then 2 else 1)
    AluD8 op v -> applyAlu op v m >> pure 2
    -- JR e (3 cycles): M1 fetch, M2 fetch immediate (prefetch), M3
    -- internal jump cycle (PC commit).
    Jr e -> do
        jumpRelative e m
        cycleNoAccess m
        pure 3
    JrCC c e -> do
        b <- testCond c m
        if b
            then do
                jumpRelative e m
                cycleNoAccess m -- M3 jump cycle (taken)
                pure 3
            else pure 2 -- not taken: M1+M2 prefetch only
    JpNN nn -> do
        -- JP nn (4 cycles): prefetch covers M1..M3, M4 is the internal
        -- jump cycle.
        cycleNoAccess m
        mapCpuRegs (\r -> r{regPC = nn}) m
        pure 4
    CallNN nn -> do
        pc <- regPC <$> getCpuRegs m
        sp <- regSP <$> getCpuRegs m
        -- CALL nn (6 cycles): prefetch covers M1..M3. M4 internal +
        -- SP--, M5 write hi, M6 write lo. Then PC := nn.
        let hi = fromIntegral (pc `shiftR` 8) :: Word8
            lo = fromIntegral (pc .&. 0xFF) :: Word8
        cycleNoAccess m -- M4
        cycleWrite (sp - 1) hi m -- M5
        cycleWrite (sp - 2) lo m -- M6
        mapCpuRegs (\r -> r{regSP = sp - 2, regPC = nn}) m
        pure 6
    Ret -> do
        -- RET (4 cycles): prefetch covers M1. M2 read low, M3 read
        -- high, M4 internal jump.
        sp <- regSP <$> getCpuRegs m
        lo <- cycleRead sp m
        hi <- cycleRead (sp + 1) m
        cycleNoAccess m
        let target = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        mapCpuRegs (\r -> r{regSP = sp + 2, regPC = target}) m
        pure 4
    PushRr s -> do
        -- PUSH rr (4 cycles): prefetch covers M1. M2 internal/SP--,
        -- M3 write hi, M4 write lo.
        v <- getReg16Stack s m
        sp <- regSP <$> getCpuRegs m
        let hi = fromIntegral (v `shiftR` 8) :: Word8
            lo = fromIntegral (v .&. 0xFF) :: Word8
        cycleNoAccess m
        cycleWrite (sp - 1) hi m
        cycleWrite (sp - 2) lo m
        mapCpuRegs (\r -> r{regSP = sp - 2}) m
        pure 4
    PopRr s -> do
        -- POP rr (3 cycles): prefetch covers M1. M2 read low, M3 read high.
        sp <- regSP <$> getCpuRegs m
        lo <- cycleRead sp m
        hi <- cycleRead (sp + 1) m
        let v = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        mapCpuRegs (\r -> r{regSP = sp + 2}) m
        setReg16Stack s v m
        pure 3
    JpCC c nn -> do
        b <- testCond c m
        if b
            then do
                cycleNoAccess m -- M4 internal jump
                mapCpuRegs (\r -> r{regPC = nn}) m
                pure 4
            else pure 3 -- not taken: M1..M3 prefetch only
    CallCC c nn -> do
        b <- testCond c m
        if b
            then do
                pc <- regPC <$> getCpuRegs m
                sp <- regSP <$> getCpuRegs m
                let hi = fromIntegral (pc `shiftR` 8) :: Word8
                    lo = fromIntegral (pc .&. 0xFF) :: Word8
                cycleNoAccess m
                cycleWrite (sp - 1) hi m
                cycleWrite (sp - 2) lo m
                mapCpuRegs (\r -> r{regSP = sp - 2, regPC = nn}) m
                pure 6
            else pure 3
    RetCC c -> do
        b <- testCond c m
        if b
            then do
                -- RET cc taken (5 cycles): prefetch M1. M2 cond check
                -- internal, M3 read low, M4 read high, M5 internal.
                cycleNoAccess m
                sp <- regSP <$> getCpuRegs m
                lo <- cycleRead sp m
                hi <- cycleRead (sp + 1) m
                cycleNoAccess m
                let target = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
                mapCpuRegs (\r -> r{regSP = sp + 2, regPC = target}) m
                pure 5
            else do
                cycleNoAccess m -- M2 cond check
                pure 2
    Reti -> do
        -- RETI: like RET, plus IME := True at the end.
        sp <- regSP <$> getCpuRegs m
        lo <- cycleRead sp m
        hi <- cycleRead (sp + 1) m
        cycleNoAccess m
        let target = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        mapCpuRegs (\r -> r{regSP = sp + 2, regPC = target}) m
        mapCpu (\c -> c{cpuIme = True}) m
        pure 4
    JpHL -> do
        hl <- getReg16 RHL m
        mapCpuRegs (\r -> r{regPC = hl}) m
        pure 1
    Rst target -> do
        -- RST n (4 cycles): prefetch M1. M2 internal, M3/M4 push PC.
        pc <- regPC <$> getCpuRegs m
        sp <- regSP <$> getCpuRegs m
        let hi = fromIntegral (pc `shiftR` 8) :: Word8
            lo = fromIntegral (pc .&. 0xFF) :: Word8
        cycleNoAccess m
        cycleWrite (sp - 1) hi m
        cycleWrite (sp - 2) lo m
        mapCpuRegs (\r -> r{regSP = sp - 2, regPC = fromIntegral target}) m
        pure 4
    AddHlRr rr -> do
        -- ADD HL, rr (2 cycles): prefetch M1, +1 internal cycle.
        hl <- getReg16 RHL m
        v <- getReg16 rr m
        zIn <- (`testBit` 7) . regF <$> getCpuRegs m
        let (r, flags) = Alu.add16 hl v zIn
        setReg16 RHL r m
        setFlagsByte (flagsToByte flags) m
        cycleNoAccess m
        pure 2
    AddSpE e -> do
        -- ADD SP, e (4 cycles): prefetch M1+M2, +2 internal cycles.
        sp <- regSP <$> getCpuRegs m
        let (r, flags) = Alu.addSP sp e
        mapCpuRegs (\rs -> rs{regSP = r}) m
        setFlagsByte (flagsToByte flags) m
        cycleNoAccess m
        cycleNoAccess m
        pure 4
    LdHlSpE e -> do
        -- LD HL, SP+e (3 cycles): prefetch M1+M2, +1 internal cycle.
        sp <- regSP <$> getCpuRegs m
        let (r, flags) = Alu.addSP sp e
        setReg16 RHL r m
        setFlagsByte (flagsToByte flags) m
        cycleNoAccess m
        pure 3
    LdSpHl -> do
        -- LD SP, HL (2 cycles): prefetch M1, +1 internal cycle.
        hl <- getReg16 RHL m
        mapCpuRegs (\r -> r{regSP = hl}) m
        cycleNoAccess m
        pure 2
    LdhNA n -> do
        a <- getReg8 RA m
        cycleWrite (0xFF00 + fromIntegral n) a m
        pure 3
    LdhAN n -> do
        v <- cycleRead (0xFF00 + fromIntegral n) m
        setReg8 RA v m
        pure 3
    LdCA -> do
        cv <- regC <$> getCpuRegs m
        a <- getReg8 RA m
        cycleWrite (0xFF00 + fromIntegral cv) a m
        pure 2
    LdAC -> do
        cv <- regC <$> getCpuRegs m
        v <- cycleRead (0xFF00 + fromIntegral cv) m
        setReg8 RA v m
        pure 2
    LdNNA nn -> do
        a <- getReg8 RA m
        cycleWrite nn a m
        pure 4
    LdANN nn -> do
        v <- cycleRead nn m
        setReg8 RA v m
        pure 4
    LdNNSp nn -> do
        sp <- regSP <$> getCpuRegs m
        let lo = fromIntegral (sp .&. 0xFF) :: Word8
            hi = fromIntegral (sp `shiftR` 8) :: Word8
        cycleWrite nn lo m
        cycleWrite (nn + 1) hi m
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
    Ei -> do
        -- EI is a no-op if IME is already enabled (or if a previous EI
        -- has already armed the toggle). Only schedule the delayed
        -- IME-set when both flags are clear; otherwise the next step's
        -- IRQ-sampling skip would inappropriately delay an already-
        -- pending interrupt by an extra instruction. Matches SameBoy
        -- 'sm83_cpu.c' line 1352.
        cpu <- getCpu m
        when (not (cpuIme cpu) && not (cpuEiDelay cpu)) $
            mapCpu (\c -> c{cpuEiDelay = True}) m
        pure 1
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
{-# INLINE ldRRcycles #-}
ldRRcycles RIndHL _ = 2
ldRRcycles _ RIndHL = 2
ldRRcycles _ _ = 1

applyInc :: Reg8 -> Machine -> IO MCycles
{-# INLINE applyInc #-}
applyInc r m = do
    v <- getReg8 r m
    cIn <- getFlagC m
    let (v', flags) = Alu.inc8 v cIn
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 3 else 1)

applyDec :: Reg8 -> Machine -> IO MCycles
{-# INLINE applyDec #-}
applyDec r m = do
    v <- getReg8 r m
    cIn <- getFlagC m
    let (v', flags) = Alu.dec8 v cIn
    setReg8 r v' m
    setFlagsByte (flagsToByte flags) m
    pure (if r == RIndHL then 3 else 1)

applyAlu :: AluOp -> Word8 -> Machine -> IO ()
{-# INLINE applyAlu #-}
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
{-# INLINE aRotate #-}
aRotate op m = do
    a <- getReg8 RA m
    let (a', flags) = op a
        flags' = flags{Alu.flagZ = False}
    setReg8 RA a' m
    setFlagsByte (flagsToByte flags') m
    pure 1

aRotateC :: (Word8 -> Bool -> (Word8, Alu.Flags)) -> Machine -> IO MCycles
{-# INLINE aRotateC #-}
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
{-# INLINE cbRotate #-}
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
{-# INLINE cbRotateC #-}
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
{-# INLINE testCond #-}
testCond c m = do
    f <- regF <$> getCpuRegs m
    pure $ case c of
        CondNZ -> not (testBit f 7)
        CondZ -> testBit f 7
        CondNC -> not (testBit f 4)
        CondC -> testBit f 4

getReg8 :: Reg8 -> Machine -> IO Word8
{-# INLINE getReg8 #-}
getReg8 r m = case r of
    RA -> regA <$> getCpuRegs m
    RB -> regB <$> getCpuRegs m
    RC -> regC <$> getCpuRegs m
    RD -> regD <$> getCpuRegs m
    RE -> regE <$> getCpuRegs m
    RH -> regH <$> getCpuRegs m
    RL -> regL <$> getCpuRegs m
    -- @(HL)@ access ticks 1 M-cycle inline (the access M-cycle of the
    -- instruction containing this read), matching SameBoy's
    -- 'cycle_read'. The bus state at the read is therefore the bus
    -- state at the END of the corresponding M-cycle, which is what
    -- mooneye PPU/timer alignment tests expect.
    RIndHL -> do
        hl <- getReg16 RHL m
        cycleRead hl m

setReg8 :: Reg8 -> Word8 -> Machine -> IO ()
{-# INLINE setReg8 #-}
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
        cycleWrite hl v m

getReg16 :: Reg16 -> Machine -> IO Word16
{-# INLINE getReg16 #-}
getReg16 r m = case r of
    RBC -> getBC <$> getCpuRegs m
    RDE -> getDE <$> getCpuRegs m
    RHL -> getHL <$> getCpuRegs m
    RSP -> regSP <$> getCpuRegs m

setReg16 :: Reg16 -> Word16 -> Machine -> IO ()
{-# INLINE setReg16 #-}
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
{-# INLINE flagsToByte #-}
flagsToByte (Alu.Flags z n h c) =
    (if z then 0x80 else 0)
        .|. (if n then 0x40 else 0)
        .|. (if h then 0x20 else 0)
        .|. (if c then 0x10 else 0)

byteToFlags :: Word8 -> Alu.Flags
{-# INLINE byteToFlags #-}
byteToFlags b =
    Alu.Flags (testBit b 7) (testBit b 6) (testBit b 5) (testBit b 4)

setFlagsByte :: Word8 -> Machine -> IO ()
{-# INLINE setFlagsByte #-}
setFlagsByte b = mapCpuRegs (\r -> r{regF = b .&. 0xF0})

getFlagC :: Machine -> IO Bool
{-# INLINE getFlagC #-}
getFlagC m = (\r -> testBit (regF r) 4) <$> getCpuRegs m
