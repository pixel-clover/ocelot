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

    describe "IRQ edge detection" $ do
        it "press of a button in the selected row latches an IRQ" $ do
            jp <- initial
            writeP1 0x10 jp -- select action row
            -- Drain the row-select edge (no buttons pressed -> no edge yet).
            _ <- takeIrqPending jp
            setButton ButtonA True jp
            edge <- takeIrqPending jp
            edge `shouldBe` True

        it "press in the unselected row does not fire" $ do
            jp <- initial
            writeP1 0x10 jp -- select action row only
            _ <- takeIrqPending jp
            setButton ButtonUp True jp -- direction row, deselected
            edge <- takeIrqPending jp
            edge `shouldBe` False

        it "row-select change exposes a held button as a falling edge" $ do
            -- Press Up first while the direction row is deselected, so
            -- the press itself does not fire an IRQ. Then switch the row
            -- selector to expose direction; the low nibble flips bit 2
            -- from 1 to 0 and that should latch a joypad-IRQ. Without
            -- this, games that use 'select row + HALT' to wait for input
            -- can miss buttons held across the row change.
            jp <- initial
            writeP1 0x10 jp -- action only
            _ <- takeIrqPending jp
            setButton ButtonUp True jp
            preEdge <- takeIrqPending jp
            preEdge `shouldBe` False
            writeP1 0x20 jp -- switch to direction; Up bit goes 1->0
            edge <- takeIrqPending jp
            edge `shouldBe` True

        it "row-select change with no held button does not fire" $ do
            jp <- initial
            writeP1 0x10 jp
            _ <- takeIrqPending jp
            writeP1 0x20 jp
            edge <- takeIrqPending jp
            edge `shouldBe` False

        it "press in shared column with both rows selected and other-row button held does not fire" $ do
            -- Regression: with both rows selected (sel=0x00), bit 0 of
            -- the low nibble is the AND of A (action row) and Right
            -- (direction row). If Right is already held (bit 0 = 0)
            -- and the user then presses A, the bit stays at 0 — no
            -- falling edge. The previous implementation always fired
            -- an IRQ on a fresh selected-row press, missing this AND
            -- interaction. SameBoy 'GB_update_joyp' computes the OR
            -- across rows and edge-detects the merged byte.
            jp <- initial
            writeP1 0x00 jp -- both rows selected
            _ <- takeIrqPending jp
            setButton ButtonRight True jp -- bit 0 -> 0 (falling edge)
            _ <- takeIrqPending jp -- drain
            setButton ButtonA True jp -- shares bit 0; no new edge
            edge <- takeIrqPending jp
            edge `shouldBe` False
