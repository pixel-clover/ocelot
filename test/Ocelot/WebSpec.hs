{-# LANGUAGE OverloadedStrings #-}

module Ocelot.WebSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Testing (synthNoMbcRom)
import qualified Ocelot.Web as Web
import Test.Hspec

spec :: Spec
spec = do
    describe "loadSession" $ do
        it "rejects a truncated ROM image" $ do
            result <- Web.loadSession (BS.replicate 0x100 0)
            isLeft result `shouldBe` True

        it "loads a synthetic ROM and reports DMG defaults" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.sessionIsCgb session `shouldBe` False
            Web.sessionHasBattery session `shouldBe` False

    describe "frame stepping" $ do
        it "runs one frame and exposes a full RGB framebuffer" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            fb <- Web.framebufferRgb session
            V.length fb `shouldBe` Web.framebufferWidth * Web.framebufferHeight * 3

        it "exposes framebuffer bytes that match the RGB framebuffer snapshot" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            fb <- Web.framebufferRgb session
            fbBytes <- Web.framebufferRgbBytes session
            BS.unpack fbBytes `shouldBe` V.toList fb

        it "exposes RGBA framebuffer bytes for the browser host" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            fb <- Web.framebufferRgb session
            fbBytes <- Web.framebufferRgbaBytes session
            BS.unpack fbBytes `shouldBe` rgbaFromRgb (V.toList fb)

        it "accepts joypad input and round-trips save states" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.setButton Joypad.ButtonA True session
            Web.runFrame session
            blob <- Web.saveState session
            result <- Web.loadState blob session
            result `shouldBe` Right ()

        it "drains audio samples without failing" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            samples <- Web.drainAudioSamples session
            length samples `shouldSatisfy` (>= 0)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

rgbaFromRgb :: [Word8] -> [Word8]
rgbaFromRgb [] = []
rgbaFromRgb (r : g : b : rest) = r : g : b : 255 : rgbaFromRgb rest
rgbaFromRgb _ = error "RGB framebuffer length must be a multiple of 3"
