{-# LANGUAGE OverloadedStrings #-}

module Ocelot.JoypadSpec (spec) where

import Data.Bits ((.&.))
import Ocelot.Joypad
import Test.Hspec

spec :: Spec
spec = do
    describe "default state" $ do
        it "matches the post-boot 0xCF value" $ do
            jp <- initial
            v <- readP1 jp
            v `shouldBe` 0xCF

    describe "row select" $ do
        it "selecting the action row exposes A/B/Select/Start in bits 0-3" $ do
            jp <- initial
            -- Pressing A; selecting the action row (write bit 5 = 0, bit 4 = 1: select action only).
            setButton ButtonA True jp
            writeP1 0x10 jp -- bit 5=0 selects action, bit 4=1 deselects direction
            v <- readP1 jp
            -- Bit 0 = 0 (A pressed), bits 1-3 = 1, top nibble pattern.
            (v .&. 0x0F) `shouldBe` 0x0E

        it "selecting the direction row exposes Right/Left/Up/Down in bits 0-3" $ do
            jp <- initial
            setButton ButtonRight True jp
            setButton ButtonUp True jp
            writeP1 0x20 jp -- bit 4=0 selects direction, bit 5=1 deselects action
            v <- readP1 jp
            -- Bit 0 = 0 (Right pressed), bit 2 = 0 (Up pressed), bits 1, 3 = 1.
            (v .&. 0x0F) `shouldBe` 0x0A

        it "no row selected reads 0xFF in the low nibble" $ do
            jp <- initial
            setButton ButtonA True jp
            writeP1 0x30 jp -- both rows deselected
            v <- readP1 jp
            (v .&. 0x0F) `shouldBe` 0x0F

    describe "release" $ do
        it "releasing a button restores its bit to 1" $ do
            jp <- initial
            setButton ButtonA True jp
            writeP1 0x10 jp
            v0 <- readP1 jp
            setButton ButtonA False jp
            v1 <- readP1 jp
            (v0 .&. 0x01) `shouldBe` 0x00
            (v1 .&. 0x01) `shouldBe` 0x01

    describe "isPressed helper" $ do
        it "tracks individual button state" $ do
            jp <- initial
            ip0 <- isPressed ButtonStart jp
            setButton ButtonStart True jp
            ip1 <- isPressed ButtonStart jp
            setButton ButtonStart False jp
            ip2 <- isPressed ButtonStart jp
            ip0 `shouldBe` False
            ip1 `shouldBe` True
            ip2 `shouldBe` False
