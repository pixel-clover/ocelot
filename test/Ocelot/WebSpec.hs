{-# LANGUAGE OverloadedStrings #-}

module Ocelot.WebSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import qualified Ocelot.Joypad as Joypad
import Ocelot.Testing (synthNoMbcRom)
import qualified Ocelot.Web as Web
import Test.Hspec

{- | 32 KiB NoMbc image whose entry point runs @prog@ at @0x0150@.

'Ocelot.Testing.synthNoMbcRom' puts its bytes at offset 0, but a cart loaded through
'Web.loadSession' starts at @0x0100@, so those bytes never execute. This lays down the
usual @NOP; JP 0x0150@ entry stub and puts the program where the stub jumps.
-}
mkRomWithProgram :: [Word8] -> BS.ByteString
mkRomWithProgram prog =
    let romSize = 32 * 1024
        v0 = V.replicate romSize 0 :: V.Vector Word8
        header =
            [ (0x0100, 0x00) -- NOP
            , (0x0101, 0xC3) -- JP 0x0150
            , (0x0102, 0x50)
            , (0x0103, 0x01)
            , (0x0143, 0x00) -- DMG-only
            , (0x0147, 0x00) -- NoMbc
            , (0x0148, 0x00)
            , (0x0149, 0x00)
            , (0x014B, 0x33)
            ]
        body0 = BS.pack (V.toList (v0 V.// header V.// zip [0x0150 ..] prog))
        cs = expectedHeaderChecksum body0
     in BS.take 0x14D body0 <> BS.singleton cs <> BS.drop 0x14E body0

{- | Increments BGP once per frame, so background colour 0 changes and the screen
repaints every frame.

Synchronising on LY is the point. An earlier version of this just incremented BGP in a
tight loop, which does not work: the loop runs about 1596 times per frame, so BGP
advances ~60 per frame, and colour 0 is selected by BGP bits 0-1 alone. 60 is a
multiple of 4, so those two bits landed on the same value every frame and the picture
never changed. Waiting for LY 144, incrementing once, then waiting for LY to leave 144
gives exactly one increment per frame.
-}
romCyclingPalette :: BS.ByteString
romCyclingPalette =
    mkRomWithProgram
        [ 0xF0
        , 0x44 -- LDH A,(FF44)   ; LY
        , 0xFE
        , 0x90 -- CP 144
        , 0x20
        , 0xFA -- JR NZ,-6       ; wait for VBlank
        , 0xF0
        , 0x47 -- LDH A,(FF47)   ; BGP
        , 0x3C -- INC A
        , 0xE0
        , 0x47 -- LDH (FF47),A
        , 0xF0
        , 0x44 -- LDH A,(FF44)
        , 0xFE
        , 0x90 -- CP 144
        , 0x28
        , 0xFA -- JR Z,-6        ; wait for VBlank to end
        , 0xC3
        , 0x50
        , 0x01 -- JP 0x0150
        ]

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

    describe "stall watchdog" $ do
        {- The watchdog exists so a frozen picture in the browser produces a report
        instead of a shrug. It counts consecutive frames whose picture is unchanged;
        crossing 'stallThreshold' is a cue to gather diagnostics, deliberately not an
        error, because a title screen or pause menu holds a still frame forever and is
        perfectly healthy. -}
        it "counts consecutive unchanged frames on a ROM that draws nothing" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            -- First frame establishes the fingerprint, so the count only rises after it.
            Web.runFrame session
            Web.runFrame session
            Web.runFrame session
            n <- Web.stalledFrames session
            n `shouldBe` 2

        it "stays at zero for a ROM whose picture keeps changing" $ do
            {- Drives BGP so colour 0 changes continuously, which repaints the whole
            screen every frame. A game that is drawing must never trip the watchdog. -}
            Right session <- Web.loadSession romCyclingPalette
            mapM_ (const (Web.runFrame session)) [1 .. 8 :: Int]
            n <- Web.stalledFrames session
            n `shouldBe` 0

        it "brackets the threshold between a brief pause and a freeze that ends in a restart" $ do
            -- Above ~2s so a momentary pause during play stays quiet, and well under 10s
            -- because a game that freezes and then restarts is only still for a moment;
            -- too long a threshold never fires for that symptom at all.
            Web.stallThreshold `shouldSatisfy` (>= 120)
            Web.stallThreshold `shouldSatisfy` (<= 300)

        it "reports machine state as text a user can paste into a bug report" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            report <- Web.debugState session
            let text = BS.unpack report
            -- The fields that distinguish a wedged guest from an idle one.
            mapM_
                (\field -> (field `BS.isInfixOf` report) `shouldBe` True)
                ["pc=", "halted=", "pending=", "lcdc=", "hdma5=", "mbc=", "stalledFrames="]
            length text `shouldSatisfy` (< 400)

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

        it "drains audio samples as a vector consistently with the list path" $ do
            let rom = synthNoMbcRom BS.empty
            Right session <- Web.loadSession rom
            Web.runFrame session
            samples <- Web.drainAudioSamplesVector session
            V.length samples `shouldSatisfy` (>= 0)
            Web.drainAudioSamples session `shouldReturn` []

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

rgbaFromRgb :: [Word8] -> [Word8]
rgbaFromRgb [] = []
rgbaFromRgb (r : g : b : rest) = r : g : b : 255 : rgbaFromRgb rest
rgbaFromRgb _ = error "RGB framebuffer length must be a multiple of 3"
