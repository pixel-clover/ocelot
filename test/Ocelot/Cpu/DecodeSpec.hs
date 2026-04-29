module Ocelot.Cpu.DecodeSpec (spec) where

import Ocelot.Cpu.Decode
import Test.Hspec

spec :: Spec
spec = do
    describe "decode" $ do
        it "0x00 is NOP" $
            decode 0x00 0 0 `shouldBe` Decoded Nop 1
        it "0x76 is HALT (the LD (HL),(HL) slot)" $
            decode 0x76 0 0 `shouldBe` Decoded Halt 1
        it "0xC9 is RET" $
            decode 0xC9 0 0 `shouldBe` Decoded Ret 1
        it "0xC3 lo hi is JP nn (little-endian)" $
            decode 0xC3 0x34 0x12 `shouldBe` Decoded (JpNN 0x1234) 3
        it "0xCD lo hi is CALL nn" $
            decode 0xCD 0x00 0x80 `shouldBe` Decoded (CallNN 0x8000) 3

    describe "LD r,r' decomposition" $ do
        it "0x40 is LD B,B" $
            decode 0x40 0 0 `shouldBe` Decoded (LdRR RB RB) 1
        it "0x47 is LD B,A" $
            decode 0x47 0 0 `shouldBe` Decoded (LdRR RB RA) 1
        it "0x70 is LD (HL),B" $
            decode 0x70 0 0 `shouldBe` Decoded (LdRR RIndHL RB) 1
        it "0x7E is LD A,(HL)" $
            decode 0x7E 0 0 `shouldBe` Decoded (LdRR RA RIndHL) 1
        it "0x7F is LD A,A" $
            decode 0x7F 0 0 `shouldBe` Decoded (LdRR RA RA) 1

    describe "LD r,d8" $ do
        it "0x06 nn is LD B,nn" $
            decode 0x06 0x42 0 `shouldBe` Decoded (LdRD8 RB 0x42) 2
        it "0x3E nn is LD A,nn" $
            decode 0x3E 0xAB 0 `shouldBe` Decoded (LdRD8 RA 0xAB) 2
        it "0x36 nn is LD (HL),nn" $
            decode 0x36 0xCD 0 `shouldBe` Decoded (LdRD8 RIndHL 0xCD) 2

    describe "LD rr,d16" $ do
        it "0x01 lo hi is LD BC,nn" $
            decode 0x01 0x34 0x12 `shouldBe` Decoded (LdRrD16 RBC 0x1234) 3
        it "0x31 lo hi is LD SP,nn" $
            decode 0x31 0xFE 0xFF `shouldBe` Decoded (LdRrD16 RSP 0xFFFE) 3

    describe "ALU r and ALU d8" $ do
        it "0x80 is ADD A,B" $
            decode 0x80 0 0 `shouldBe` Decoded (AluR AluAdd RB) 1
        it "0x86 is ADD A,(HL)" $
            decode 0x86 0 0 `shouldBe` Decoded (AluR AluAdd RIndHL) 1
        it "0xBF is CP A" $
            decode 0xBF 0 0 `shouldBe` Decoded (AluR AluCp RA) 1
        it "0xC6 nn is ADD A,nn" $
            decode 0xC6 0x10 0 `shouldBe` Decoded (AluD8 AluAdd 0x10) 2
        it "0xFE nn is CP nn" $
            decode 0xFE 0xFF 0 `shouldBe` Decoded (AluD8 AluCp 0xFF) 2

    describe "INC / DEC" $ do
        it "0x04 is INC B" $
            decode 0x04 0 0 `shouldBe` Decoded (IncR RB) 1
        it "0x35 is DEC (HL)" $
            decode 0x35 0 0 `shouldBe` Decoded (DecR RIndHL) 1
        it "0x03 is INC BC" $
            decode 0x03 0 0 `shouldBe` Decoded (IncRr RBC) 1
        it "0x3B is DEC SP" $
            decode 0x3B 0 0 `shouldBe` Decoded (DecRr RSP) 1

    describe "JR" $ do
        it "0x18 nn is JR e (signed)" $
            decode 0x18 0xFE 0 `shouldBe` Decoded (Jr (-2)) 2
        it "0x20 nn is JR NZ,e" $
            decode 0x20 0x05 0 `shouldBe` Decoded (JrCC CondNZ 5) 2

    describe "PUSH / POP" $ do
        it "0xC5 is PUSH BC" $
            decode 0xC5 0 0 `shouldBe` Decoded (PushRr SBC) 1
        it "0xF1 is POP AF" $
            decode 0xF1 0 0 `shouldBe` Decoded (PopRr SAF) 1

    describe "Unknown" $ do
        it "0xCB (CB-prefix) decodes as Unknown for now" $
            decode 0xCB 0 0 `shouldBe` Decoded (Unknown 0xCB) 1
        it "0xD3 (no instruction) decodes as Unknown" $
            decode 0xD3 0 0 `shouldBe` Decoded (Unknown 0xD3) 1
