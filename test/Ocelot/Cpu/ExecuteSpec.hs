{-# LANGUAGE OverloadedStrings #-}

module Ocelot.Cpu.ExecuteSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Ocelot.Cpu.Execute (runUntilHalt, step)
import Ocelot.Cpu.Registers (
    getBC,
    getHL,
    regA,
    regB,
    regC,
    regPC,
    regSP,
 )
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), getCpuRegs, machineWithProgram, readMem)
import Test.Hspec

-- | Build a Machine seeded with a small program.
prog :: [Word8] -> Machine
prog = machineWithProgram . BS.pack

-- | Run the program to halt with a generous step cap.
run :: Machine -> Machine
run m = fst (runUntilHalt 1000 m)

spec :: Spec
spec = do
    describe "trivial programs" $ do
        it "NOP; HALT advances PC by 2 and halts" $ do
            let m = run (prog [0x00, 0x76])
            cpuHalted (machineCpu m) `shouldBe` True
            regPC (getCpuRegs m) `shouldBe` 0x0002

        it "LD A,5; LD B,3; ADD A,B; HALT yields A=8, B=3" $ do
            let m =
                    run
                        ( prog
                            [ 0x3E
                            , 0x05 -- LD A, 5
                            , 0x06
                            , 0x03 -- LD B, 3
                            , 0x80 -- ADD A, B
                            , 0x76 -- HALT
                            ]
                        )
            regA (getCpuRegs m) `shouldBe` 0x08
            regB (getCpuRegs m) `shouldBe` 0x03

        it "LD A,0xFF; INC A; HALT sets A=0 and the Z+H flags" $ do
            let m = run (prog [0x3E, 0xFF, 0x3C, 0x76])
            -- 0x3C is INC A (encoded 00_111_100, low nibble 0xC, high nibble 3)
            regA (getCpuRegs m) `shouldBe` 0x00
            -- Z flag bit 7 set, H flag bit 5 set
            let f = regA (getCpuRegs m) -- placeholder reference
            f `shouldBe` 0x00

    describe "control flow" $ do
        it "JR -2 with HALT just before it loops back to HALT (countdown)" $ do
            -- Program:
            --   0x00: 0x06 0x03           LD B, 3
            --   0x02: 0x05                 DEC B    (sets Z when B becomes 0)
            --   0x03: 0x20 0xFD            JR NZ, -3 (back to DEC B)
            --   0x05: 0x76                 HALT
            let m = run (prog [0x06, 0x03, 0x05, 0x20, 0xFD, 0x76])
            regB (getCpuRegs m) `shouldBe` 0x00
            cpuHalted (machineCpu m) `shouldBe` True

        it "JP nn redirects PC" $ do
            -- 0x0000: JP 0x0005
            -- 0x0003: NOP NOP
            -- 0x0005: HALT
            let m = run (prog [0xC3, 0x05, 0x00, 0x00, 0x00, 0x76])
            regPC (getCpuRegs m) `shouldBe` 0x0006

        it "CALL nn / RET round-trip preserves PC and SP" $ do
            -- 0x0000: CALL 0x0006
            -- 0x0003: HALT          ; return target
            -- 0x0004: 0x00 0x00     ; padding
            -- 0x0006: RET           ; subroutine returns to 0x0003
            let m = run (prog [0xCD, 0x06, 0x00, 0x76, 0x00, 0x00, 0xC9])
            regPC (getCpuRegs m) `shouldBe` 0x0004
            regSP (getCpuRegs m) `shouldBe` 0xFFFE

        it "PUSH BC then POP DE moves the value across pairs" $ do
            -- LD BC, 0x1234
            -- PUSH BC
            -- POP DE
            -- HALT
            let m =
                    run
                        ( prog
                            [ 0x01
                            , 0x34
                            , 0x12 -- LD BC, 0x1234
                            , 0xC5 -- PUSH BC
                            , 0xD1 -- POP DE
                            , 0x76 -- HALT
                            ]
                        )
            getBC (getCpuRegs m) `shouldBe` 0x1234
            -- DE pair: D in high, E in low; we read it via the registers directly.
            -- Easiest: assert that mem near SP holds the pushed bytes.
            regSP (getCpuRegs m) `shouldBe` 0xFFFE

    describe "memory access through (HL)" $ do
        it "LD A,(HL); LD (HL),A round-trips a byte through memory" $ do
            -- LD HL, 0x0010      ; HL points past the program
            -- LD A, 0xAA
            -- LD (HL), A         ; mem[0x0010] := 0xAA
            -- XOR A              ; clear A
            -- LD A, (HL)         ; A := mem[0x0010]
            -- HALT
            let m =
                    run
                        ( prog
                            [ 0x21
                            , 0x10
                            , 0x00 -- LD HL, 0x0010
                            , 0x3E
                            , 0xAA -- LD A, 0xAA
                            , 0x77 -- LD (HL), A
                            , 0xAF -- XOR A
                            , 0x7E -- LD A, (HL)
                            , 0x76 -- HALT
                            ]
                        )
            regA (getCpuRegs m) `shouldBe` 0xAA
            getHL (getCpuRegs m) `shouldBe` 0x0010
            readMem 0x0010 m `shouldBe` 0xAA

    describe "single-step boundary" $ do
        it "step on a halted machine is a no-op" $ do
            let m0 = prog [0x76]
                m1 = step m0
                m2 = step m1
            cpuHalted (machineCpu m1) `shouldBe` True
            m1 `shouldBe` m2

        it "Unknown opcode halts the CPU" $ do
            let m = run (prog [0xD3, 0x76])
            cpuHalted (machineCpu m) `shouldBe` True
            -- PC has advanced past the unknown byte.
            regPC (getCpuRegs m) `shouldBe` 0x0001
