{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cpu.ExecuteSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Ocelot.Cpu.Execute (runUntilHalt, step)
import Ocelot.Cpu.Registers (
    getBC,
    getHL,
    regA,
    regB,
    regF,
    regH,
    regPC,
    regSP,
 )
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), getCpu, getCpuRegs, readMem)
import Ocelot.Testing (machineWithProgram)
import Test.Hspec

prog :: [Word8] -> IO Machine
prog = machineWithProgram . BS.pack

run :: Machine -> IO Machine
run m = runUntilHalt 1000 m >> pure m

spec :: Spec
spec = do
    describe "trivial programs" $ do
        it "NOP; HALT advances PC by 2 and halts" $ do
            m <- prog [0x00, 0x76]
            _ <- run m
            cpu <- getCpu m
            cpuHalted cpu `shouldBe` True
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0002

        it "LD A,5; LD B,3; ADD A,B; HALT yields A=8, B=3" $ do
            m <- prog [0x3E, 0x05, 0x06, 0x03, 0x80, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x08
            regB regs `shouldBe` 0x03

        it "LD A,0xFF; INC A; HALT sets A=0" $ do
            m <- prog [0x3E, 0xFF, 0x3C, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x00

    describe "control flow" $ do
        it "JR -2 with HALT just before it loops back to HALT (countdown)" $ do
            m <- prog [0x06, 0x03, 0x05, 0x20, 0xFD, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regB regs `shouldBe` 0x00
            cpu <- getCpu m
            cpuHalted cpu `shouldBe` True

        it "JP nn redirects PC" $ do
            m <- prog [0xC3, 0x05, 0x00, 0x00, 0x00, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0006

        it "CALL nn / RET round-trip preserves PC and SP" $ do
            m <- prog [0xCD, 0x06, 0x00, 0x76, 0x00, 0x00, 0xC9]
            _ <- run m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0004
            regSP regs `shouldBe` 0xFFFE

        it "PUSH BC then POP DE moves the value across pairs" $ do
            m <- prog [0x01, 0x34, 0x12, 0xC5, 0xD1, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            getBC regs `shouldBe` 0x1234
            regSP regs `shouldBe` 0xFFFE

    describe "memory access through (HL)" $ do
        it "LD A,(HL); LD (HL),A round-trips a byte through WRAM" $ do
            m <-
                prog
                    [ 0x21
                    , 0x00
                    , 0xC0
                    , 0x3E
                    , 0xAA
                    , 0x77
                    , 0xAF
                    , 0x7E
                    , 0x76
                    ]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0xAA
            getHL regs `shouldBe` 0xC000
            v <- readMem 0xC000 m
            v `shouldBe` 0xAA

    describe "single-step boundary" $ do
        it "step on a halted machine ticks the bus but does not advance PC" $ do
            m <- prog [0x76]
            step m
            regs1 <- getCpuRegs m
            cpu1 <- getCpu m
            step m
            regs2 <- getCpuRegs m
            cpu2 <- getCpu m
            cpuHalted cpu1 `shouldBe` True
            cpuHalted cpu2 `shouldBe` True
            regPC regs1 `shouldBe` regPC regs2

        it "Unknown opcode halts the CPU" $ do
            m <- prog [0xD3, 0x76]
            _ <- run m
            cpu <- getCpu m
            cpuHalted cpu `shouldBe` True
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0001

    describe "conditional branches" $ do
        it "JP NZ,nn does not jump when Z is set" $ do
            m <- prog [0x3E, 0x00, 0xB7, 0xC2, 0x0A, 0x00, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0007

        it "CALL NZ,nn / RET round-trip when Z is clear" $ do
            m <-
                prog
                    [ 0x3E
                    , 0x01
                    , 0xB7
                    , 0xC4
                    , 0x09
                    , 0x00
                    , 0x76
                    , 0x00
                    , 0x00
                    , 0xC9
                    ]
            _ <- run m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0007
            regSP regs `shouldBe` 0xFFFE

        it "RST 0x08 jumps to 0x0008 and pushes the return address" $ do
            m <- prog [0xCF, 0x76, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0009

    describe "16-bit arithmetic" $ do
        it "ADD HL,BC adds and lands in HL" $ do
            m <- prog [0x21, 0x0F, 0x0F, 0x01, 0x01, 0x01, 0x09, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regH regs `shouldBe` 0x10

    describe "A-rotates" $ do
        it "RLCA on 0x80 produces A=0x01 with C=1, Z always cleared" $ do
            m <- prog [0x3E, 0x80, 0x07, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x01
            (regF regs .&. 0xF0) `shouldBe` 0x10

        it "RLCA on 0x00 still clears Z (the SM83 quirk)" $ do
            m <- prog [0x3E, 0x00, 0x07, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x00
            (regF regs .&. 0x80) `shouldBe` 0x00

    describe "CB-prefix" $ do
        it "SWAP A swaps nibbles" $ do
            m <- prog [0x3E, 0xAB, 0xCB, 0x37, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0xBA

        it "BIT 7,A sets Z when bit 7 of A is clear" $ do
            m <- prog [0x3E, 0x00, 0xCB, 0x7F, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            (regF regs .&. 0x80) `shouldBe` 0x80

        it "RES 0,A then SET 7,A" $ do
            m <- prog [0x3E, 0xFF, 0xCB, 0x87, 0xCB, 0xFF, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0xFE

    describe "DAA" $ do
        it "DAA after LD A,0x09 + 0x01 yields 0x10 (BCD)" $ do
            m <- prog [0x3E, 0x09, 0xC6, 0x01, 0x27, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x10

        it "DAA after LD A,0x99 + 0x01 yields 0x00 with C=1" $ do
            m <- prog [0x3E, 0x99, 0xC6, 0x01, 0x27, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x00
            (regF regs .&. 0x10) `shouldBe` 0x10

    describe "CPL/SCF/CCF" $ do
        it "CPL flips all A bits and sets N=H=1" $ do
            m <- prog [0x3E, 0xA5, 0x2F, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            regA regs `shouldBe` 0x5A
            (regF regs .&. 0x60) `shouldBe` 0x60

        it "SCF sets C and clears N,H" $ do
            m <- prog [0x37, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            (regF regs .&. 0xF0) `shouldBe` 0x10

        it "CCF flips C" $ do
            m <- prog [0x37, 0x3F, 0x76]
            _ <- run m
            regs <- getCpuRegs m
            (regF regs .&. 0x10) `shouldBe` 0x00

    describe "DI/EI" $ do
        it "DI clears IME, EI sets IME (after the next-instruction delay)" $ do
            m1 <- prog [0xFB, 0x76]
            _ <- run m1
            cpu1 <- getCpu m1
            cpuIme cpu1 `shouldBe` True
            m2 <- prog [0xFB, 0xF3, 0x76]
            _ <- run m2
            cpu2 <- getCpu m2
            cpuIme cpu2 `shouldBe` False
