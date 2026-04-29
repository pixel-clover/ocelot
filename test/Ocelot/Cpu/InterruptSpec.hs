{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cpu.InterruptSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Ocelot.Cpu.Execute (interruptVector, pendingInterrupt, runFor, step)
import Ocelot.Cpu.Registers (regPC, regSP)
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), getCpu, getCpuRegs, mapCpu, readMem, writeMem)
import Ocelot.Testing (machineWithProgram)
import Test.Hspec

mkProg :: [Word8] -> IO Machine
mkProg = machineWithProgram . BS.pack

setIME :: Bool -> Machine -> IO ()
setIME b = mapCpu (\c -> c{cpuIme = b})

setIfIe :: Word8 -> Word8 -> Machine -> IO ()
setIfIe iflag ie m = do
    writeMem 0xFFFF ie m
    writeMem 0xFF0F iflag m

spec :: Spec
spec = do
    describe "pendingInterrupt" $ do
        it "Nothing when IF & IE is zero" $ do
            m <- mkProg [0x00]
            r <- pendingInterrupt m
            r `shouldBe` Nothing

        it "lowest set bit wins when several are pending and enabled" $ do
            m <- mkProg [0x00]
            setIfIe 0x05 0x05 m
            r <- pendingInterrupt m
            r `shouldBe` Just 0

        it "a flagged but disabled bit is not pending" $ do
            m <- mkProg [0x00]
            setIfIe 0x04 0x01 m
            r <- pendingInterrupt m
            r `shouldBe` Nothing

    describe "interruptVector" $ do
        it "maps each index to its handler address" $
            map interruptVector [0 .. 4]
                `shouldBe` [0x40, 0x48, 0x50, 0x58, 0x60]

    describe "servicing" $ do
        it "step with IME and a Timer pending jumps to 0x0050 and clears IF bit" $ do
            m <- mkProg [0x00, 0x76]
            setIME True m
            setIfIe 0x04 0x04 m
            step m
            regs <- getCpuRegs m
            cpu <- getCpu m
            regPC regs `shouldBe` 0x0050
            cpuIme cpu `shouldBe` False
            iflag <- readMem 0xFF0F m
            (iflag .&. 0x04) `shouldBe` 0x00
            regSP regs `shouldBe` 0xFFFC

        it "without IME, step does not service the interrupt" $ do
            m <- mkProg [0x00, 0x76]
            setIfIe 0x04 0x04 m
            step m
            regs <- getCpuRegs m
            regPC regs `shouldBe` 0x0001
            iflag <- readMem 0xFF0F m
            (iflag .&. 0x04) `shouldBe` 0x04

    describe "HALT wait-for-interrupt" $ do
        it "halted machine wakes when interrupt becomes pending and IME is set" $ do
            m <- mkProg [0x76, 0x00, 0x00]
            _ <- runFor 1 m
            cpu <- getCpu m
            cpuHalted cpu `shouldBe` True
            setIME True m
            setIfIe 0x04 0x04 m
            step m
            cpu' <- getCpu m
            regs <- getCpuRegs m
            cpuHalted cpu' `shouldBe` False
            regPC regs `shouldBe` 0x0050

        it "halted machine wakes (without service) when IME is clear" $ do
            m <- mkProg [0x76, 0x00, 0x00]
            _ <- runFor 1 m
            setIfIe 0x04 0x04 m
            step m
            cpu' <- getCpu m
            regs <- getCpuRegs m
            cpuHalted cpu' `shouldBe` False
            regPC regs `shouldBe` 0x0001

        it "HALT bug: with IME=0 and IF&IE != 0, HALT does not halt" $ do
            -- Without the bug fix the CPU halts and never resumes (because
            -- service requires IME). After the fix, HALT in this state is
            -- a no-op and execution continues with the next instruction.
            m <- mkProg [0x76, 0x00, 0x00]
            setIfIe 0x04 0x04 m
            -- IME is False by default.
            _ <- runFor 1 m
            cpu <- getCpu m
            regs <- getCpuRegs m
            cpuHalted cpu `shouldBe` False
            regPC regs `shouldBe` 0x0001

        it "HALT with IME=0 and no pending IRQ still halts normally" $ do
            m <- mkProg [0x76, 0x00, 0x00]
            -- IF & IE both zero; IME irrelevant for halting itself.
            _ <- runFor 1 m
            cpu <- getCpu m
            cpuHalted cpu `shouldBe` True

    describe "EI delay" $ do
        it "EI does not enable IME until after the next instruction" $ do
            m <- mkProg [0xFB, 0x00, 0x00, 0x76]
            step m
            cpu1 <- getCpu m
            step m
            cpu2 <- getCpu m
            cpuIme cpu1 `shouldBe` False
            cpuEiDelay cpu1 `shouldBe` True
            cpuIme cpu2 `shouldBe` True
            cpuEiDelay cpu2 `shouldBe` False

        it "interrupts pending the moment EI runs do not service before the next instruction" $ do
            m <- mkProg [0xFB, 0x00, 0x76]
            setIfIe 0x04 0x04 m
            step m
            regs1 <- getCpuRegs m
            step m
            regs2 <- getCpuRegs m
            step m
            regs3 <- getCpuRegs m
            regPC regs1 `shouldBe` 0x0001
            regPC regs2 `shouldBe` 0x0002
            regPC regs3 `shouldBe` 0x0050
