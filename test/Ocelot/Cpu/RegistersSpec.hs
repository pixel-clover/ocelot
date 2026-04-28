{-# OPTIONS_GHC -Wno-orphans #-}

module Ocelot.Cpu.RegistersSpec (spec) where

import Data.Bits ((.&.))
import Data.Word (Word16)
import Ocelot.Cpu.Registers
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

spec :: Spec
spec = do
    describe "16-bit register pairs" $ do
        it "BC round-trips through getBC . setBC" $
            property $
                \w -> getBC (setBC w zeroRegisters) === (w :: Word16)
        it "DE round-trips through getDE . setDE" $
            property $
                \w -> getDE (setDE w zeroRegisters) === (w :: Word16)
        it "HL round-trips through getHL . setHL" $
            property $
                \w -> getHL (setHL w zeroRegisters) === (w :: Word16)
        it "AF round-trips with the low nibble of F masked to zero" $
            property $
                \w -> getAF (setAF w zeroRegisters) === (w .&. 0xFFF0 :: Word16)

    describe "flags" $ do
        it "setFlag _ b then getFlag _ is b" $
            property $
                \f b -> getFlag f (setFlag f b zeroRegisters) === b
        it "setting one flag leaves the other three unchanged" $
            property $
                \f1 f2 b ->
                    f1 /= f2 ==>
                        getFlag f2 (setFlag f1 b zeroRegisters) === False
        it "the low nibble of F is always zero after setFlag" $
            property $
                \f b -> regF (setFlag f b zeroRegisters) .&. 0x0F === 0

    describe "DMG post-boot register state" $ do
        it "matches documented post-boot values" $ do
            regA dmgPostBoot `shouldBe` 0x01
            regF dmgPostBoot `shouldBe` 0xB0
            getBC dmgPostBoot `shouldBe` 0x0013
            getDE dmgPostBoot `shouldBe` 0x00D8
            getHL dmgPostBoot `shouldBe` 0x014D
            regSP dmgPostBoot `shouldBe` 0xFFFE
            regPC dmgPostBoot `shouldBe` 0x0100
        it "has Z and C set, N and H clear" $ do
            getFlag FlagZ dmgPostBoot `shouldBe` True
            getFlag FlagN dmgPostBoot `shouldBe` False
            getFlag FlagH dmgPostBoot `shouldBe` True
            getFlag FlagC dmgPostBoot `shouldBe` True

instance Arbitrary Flag where
    arbitrary = elements [FlagZ, FlagN, FlagH, FlagC]
