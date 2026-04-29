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

    describe "conditional branches" $ do
        it "0xC0 is RET NZ" $ decode 0xC0 0 0 `shouldBe` Decoded (RetCC CondNZ) 1
        it "0xD9 is RETI" $ decode 0xD9 0 0 `shouldBe` Decoded Reti 1
        it "0xC2 lo hi is JP NZ,nn" $
            decode 0xC2 0x34 0x12 `shouldBe` Decoded (JpCC CondNZ 0x1234) 3
        it "0xCC lo hi is CALL Z,nn" $
            decode 0xCC 0x00 0x80 `shouldBe` Decoded (CallCC CondZ 0x8000) 3
        it "0xE9 is JP (HL)" $ decode 0xE9 0 0 `shouldBe` Decoded JpHL 1

    describe "high-page and direct addressing" $ do
        it "0xE0 nn is LDH (n),A" $
            decode 0xE0 0x44 0 `shouldBe` Decoded (LdhNA 0x44) 2
        it "0xF0 nn is LDH A,(n)" $
            decode 0xF0 0x44 0 `shouldBe` Decoded (LdhAN 0x44) 2
        it "0xE2 is LD (C),A" $ decode 0xE2 0 0 `shouldBe` Decoded LdCA 1
        it "0xEA lo hi is LD (nn),A" $
            decode 0xEA 0x00 0xC0 `shouldBe` Decoded (LdNNA 0xC000) 3
        it "0x08 lo hi is LD (nn),SP" $
            decode 0x08 0x00 0xD0 `shouldBe` Decoded (LdNNSp 0xD000) 3

    describe "16-bit arithmetic and SP ops" $ do
        it "0x09 is ADD HL,BC" $ decode 0x09 0 0 `shouldBe` Decoded (AddHlRr RBC) 1
        it "0xE8 nn is ADD SP,e" $
            decode 0xE8 0xFE 0 `shouldBe` Decoded (AddSpE (-2)) 2
        it "0xF8 nn is LD HL,SP+e" $
            decode 0xF8 0x05 0 `shouldBe` Decoded (LdHlSpE 5) 2
        it "0xF9 is LD SP,HL" $ decode 0xF9 0 0 `shouldBe` Decoded LdSpHl 1

    describe "A-rotates and miscellaneous" $ do
        it "0x07 is RLCA" $ decode 0x07 0 0 `shouldBe` Decoded Rlca 1
        it "0x17 is RLA" $ decode 0x17 0 0 `shouldBe` Decoded Rla 1
        it "0x27 is DAA" $ decode 0x27 0 0 `shouldBe` Decoded Daa 1
        it "0x2F is CPL" $ decode 0x2F 0 0 `shouldBe` Decoded Cpl 1
        it "0x37 is SCF" $ decode 0x37 0 0 `shouldBe` Decoded Scf 1
        it "0x3F is CCF" $ decode 0x3F 0 0 `shouldBe` Decoded Ccf 1
        it "0xF3 is DI; 0xFB is EI" $ do
            decode 0xF3 0 0 `shouldBe` Decoded Di 1
            decode 0xFB 0 0 `shouldBe` Decoded Ei 1

    describe "RST" $ do
        it "0xC7 is RST 0x00" $ decode 0xC7 0 0 `shouldBe` Decoded (Rst 0x00) 1
        it "0xFF is RST 0x38" $ decode 0xFF 0 0 `shouldBe` Decoded (Rst 0x38) 1

    describe "CB-prefix block" $ do
        it "0xCB 0x00 is RLC B" $ decode 0xCB 0x00 0 `shouldBe` Decoded (Rlc RB) 2
        it "0xCB 0x06 is RLC (HL)" $
            decode 0xCB 0x06 0 `shouldBe` Decoded (Rlc RIndHL) 2
        it "0xCB 0x37 is SWAP A" $
            decode 0xCB 0x37 0 `shouldBe` Decoded (Swap RA) 2
        it "0xCB 0x40 is BIT 0,B" $
            decode 0xCB 0x40 0 `shouldBe` Decoded (BitOp 0 RB) 2
        it "0xCB 0x7F is BIT 7,A" $
            decode 0xCB 0x7F 0 `shouldBe` Decoded (BitOp 7 RA) 2
        it "0xCB 0x86 is RES 0,(HL)" $
            decode 0xCB 0x86 0 `shouldBe` Decoded (Res 0 RIndHL) 2
        it "0xCB 0xFF is SET 7,A" $
            decode 0xCB 0xFF 0 `shouldBe` Decoded (Set 7 RA) 2

    describe "Unknown" $ do
        it "0xD3 (no instruction) decodes as Unknown" $
            decode 0xD3 0 0 `shouldBe` Decoded (Unknown 0xD3) 1
        it "0xDD (no instruction) decodes as Unknown" $
            decode 0xDD 0 0 `shouldBe` Decoded (Unknown 0xDD) 1
