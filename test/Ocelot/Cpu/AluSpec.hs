{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ocelot.Cpu.AluSpec (spec) where

import Data.Bits ((.&.))
import Data.Int (Int8)
import Data.Word (Word16, Word32, Word8)
import Ocelot.Cpu.Alu
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

spec :: Spec
spec = do
    describe "add8" $ do
        it "0x00 + 0x00 sets Z" $
            add8 0x00 0x00 `shouldBe` (0x00, Flags True False False False)
        it "0x0F + 0x01 sets H but not C" $
            add8 0x0F 0x01 `shouldBe` (0x10, Flags False False True False)
        it "0xFF + 0x01 sets Z, H, C" $
            add8 0xFF 0x01 `shouldBe` (0x00, Flags True False True True)
        prop "result equals (a + b) mod 256" $ \a b ->
            fst (add8 a b) === a + b
        prop "N is always False" $ \a b ->
            flagN (snd (add8 a b)) === False
        prop "Z iff result is zero" $ \a b ->
            flagZ (snd (add8 a b)) === (fst (add8 a b) == 0)
        prop "H matches the low-nibble carry" $ \a b ->
            flagH (snd (add8 a b))
                === ((a .&. 0x0F) + (b .&. 0x0F) > 0x0F)
        prop "C matches the unsigned overflow" $ \a b ->
            flagC (snd (add8 a b))
                === ((fromIntegral a + fromIntegral b :: Word16) > 0xFF)

    describe "adc8" $ do
        it "0xFF + 0x00 with carry-in sets Z, H, C" $
            adc8 0xFF 0x00 True `shouldBe` (0x00, Flags True False True True)
        it "0x0E + 0x01 with carry-in sets H" $
            adc8 0x0E 0x01 True `shouldBe` (0x10, Flags False False True False)
        prop "with carry-in False, adc8 a b False == add8 a b" $ \a b ->
            adc8 a b False === add8 a b

    describe "sub8" $ do
        it "0x10 - 0x01 sets N and H" $
            sub8 0x10 0x01 `shouldBe` (0x0F, Flags False True True False)
        it "0x00 - 0x01 sets N, H, C" $
            sub8 0x00 0x01 `shouldBe` (0xFF, Flags False True True True)
        it "0x05 - 0x05 sets Z and N" $
            sub8 0x05 0x05 `shouldBe` (0x00, Flags True True False False)
        prop "result equals (a - b) mod 256" $ \a b ->
            fst (sub8 a b) === a - b
        prop "N is always True" $ \a b ->
            flagN (snd (sub8 a b)) === True
        prop "C iff b > a" $ \a b ->
            flagC (snd (sub8 a b)) === (a < b)

    describe "sbc8" $ do
        it "0x00 - 0x00 with carry-in sets N, H, C" $
            sbc8 0x00 0x00 True `shouldBe` (0xFF, Flags False True True True)
        prop "with carry-in False, sbc8 a b False == sub8 a b" $ \a b ->
            sbc8 a b False === sub8 a b

    describe "and8" $ do
        it "0xF0 .&. 0x0F sets Z and H" $
            and8 0xF0 0x0F `shouldBe` (0x00, Flags True False True False)
        it "0xF0 .&. 0xF0 sets only H" $
            and8 0xF0 0xF0 `shouldBe` (0xF0, Flags False False True False)
        prop "H is always True; N and C are always False" $ \a b ->
            let f = snd (and8 a b)
             in (flagH f, flagN f, flagC f) === (True, False, False)

    describe "or8" $ do
        it "0xF0 .|. 0x0F clears all flags except none" $
            or8 0xF0 0x0F `shouldBe` (0xFF, Flags False False False False)
        it "0x00 .|. 0x00 sets Z" $
            or8 0x00 0x00 `shouldBe` (0x00, Flags True False False False)
        prop "all non-Z flags are False" $ \a b ->
            let f = snd (or8 a b)
             in (flagN f, flagH f, flagC f) === (False, False, False)

    describe "xor8" $ do
        it "0xFF xor 0xFF sets Z" $
            xor8 0xFF 0xFF `shouldBe` (0x00, Flags True False False False)
        prop "all non-Z flags are False" $ \a b ->
            let f = snd (xor8 a b)
             in (flagN f, flagH f, flagC f) === (False, False, False)

    describe "cp8" $ do
        prop "matches the flag side of sub8" $ \a b ->
            cp8 a b === snd (sub8 a b)

    describe "inc8" $ do
        it "0xFF + 1 with carry-in clear sets Z, H; C remains clear" $
            inc8 0xFF False `shouldBe` (0x00, Flags True False True False)
        it "0x0F + 1 with carry-in set: H, C preserved" $
            inc8 0x0F True `shouldBe` (0x10, Flags False False True True)
        prop "C is preserved" $ \v c ->
            flagC (snd (inc8 v c)) === c
        prop "N is always False" $ \v c ->
            flagN (snd (inc8 v c)) === False

    describe "dec8" $ do
        it "0x01 - 1 with carry-in set: Z, N; C preserved" $
            dec8 0x01 True `shouldBe` (0x00, Flags True True False True)
        it "0x10 - 1 with carry-in clear: N, H; C preserved" $
            dec8 0x10 False `shouldBe` (0x0F, Flags False True True False)
        it "0x00 - 1 with carry-in set: N, H; C preserved" $
            dec8 0x00 True `shouldBe` (0xFF, Flags False True True True)
        prop "C is preserved" $ \v c ->
            flagC (snd (dec8 v c)) === c
        prop "N is always True" $ \v c ->
            flagN (snd (dec8 v c)) === True

    describe "add16" $ do
        it "0x0FFF + 0x0001 with Z=False sets H" $
            add16 0x0FFF 0x0001 False
                `shouldBe` (0x1000, Flags False False True False)
        it "0xFFFF + 0x0001 with Z=True sets H, C; Z preserved" $
            add16 0xFFFF 0x0001 True
                `shouldBe` (0x0000, Flags True False True True)
        prop "result equals (a + b) mod 65536" $ \a b z ->
            fst (add16 a b z) === a + b
        prop "Z is preserved" $ \a b z ->
            flagZ (snd (add16 a b z)) === z
        prop "C matches the 16-bit unsigned overflow" $ \a b z ->
            flagC (snd (add16 a b z))
                === ((fromIntegral a + fromIntegral b :: Word32) > 0xFFFF)

    describe "addSP" $ do
        it "0x0000 + 1 has no carry, no half-carry" $
            addSP 0x0000 1
                `shouldBe` (0x0001, Flags False False False False)
        it "0x000F + 1 sets H" $
            addSP 0x000F 1
                `shouldBe` (0x0010, Flags False False True False)
        it "0x00FF + 1 sets H and C" $
            addSP 0x00FF 1
                `shouldBe` (0x0100, Flags False False True True)
        it "0x0000 - 1 wraps to 0xFFFF, no half-carry, no carry" $
            addSP 0x0000 (-1)
                `shouldBe` (0xFFFF, Flags False False False False)
        it "0x0080 + (-1) yields 0x007F with C set" $
            addSP 0x0080 (-1)
                `shouldBe` (0x007F, Flags False False False True)
        prop "Z and N are always False" $ \sp (e :: Int8) ->
            let f = snd (addSP sp e)
             in (flagZ f, flagN f) === (False, False)
