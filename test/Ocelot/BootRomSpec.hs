{-# LANGUAGE OverloadedStrings #-}

{- | Boot-ROM handoff semantics.

When a boot ROM is installed via 'machineFromCartridgeWithBoot', peripheral state at PC=0 must
reflect real-hardware power-on, not Ocelot's post-boot shortcut. Specifically the LCD must be off
(LCDC bit 7 clear) so the PPU's LY register stays at 0 until the boot ROM itself writes a non-zero LCDC.
Without this, every boot-ROM-driven differential trace drifts in PPU state within a few hundred
T-cycles of cart entry, and a hypothetical real boot ROM run on Ocelot would observe its LCD
already on before it asked for it.
-}
module Ocelot.BootRomSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import qualified Ocelot.Machine as Machine
import Test.Hspec

mkCgbRom :: BS.ByteString
mkCgbRom =
    let romSize = 32 * 1024
        v0 = V.replicate romSize 0 :: V.Vector Word8
        title = "BOOT"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (BSC.pack title `BS.append` BS.replicate 16 0)))
        fields =
            [ (0x0100, 0x00)
            , (0x0101, 0xC3)
            , (0x0102, 0x50)
            , (0x0103, 0x01)
            , (0x0143, 0x80) -- DmgAndCgb
            , (0x0147, 0x00)
            , (0x0148, 0x00)
            , (0x0149, 0x00)
            , (0x014B, 0x33)
            ]
                <> titleBytes
        v1 = v0 V.// fields
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.take 0x14D body0 <> BS.singleton cs <> BS.drop 0x14E body0

-- | A boot ROM that does nothing except sit at PC=0 (NOPs forever).
trivialBootStub :: BS.ByteString
trivialBootStub = BS.replicate 0x100 0x00

spec :: Spec
spec = do
    describe "Machine constructed with a boot ROM" $ do
        it "starts with LCD off (LCDC=0x00) so PPU does not tick during boot" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridgeWithBoot (Just trivialBootStub) cart
            lcdc <- Bus.read8 0xFF40 (Machine.machineBus m)
            lcdc `shouldBe` 0x00

        it "starts with BGP, OBP0, OBP1 all 0x00 (boot ROM responsible for setting palettes)" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridgeWithBoot (Just trivialBootStub) cart
            bgp <- Bus.read8 0xFF47 (Machine.machineBus m)
            obp0 <- Bus.read8 0xFF48 (Machine.machineBus m)
            obp1 <- Bus.read8 0xFF49 (Machine.machineBus m)
            (bgp, obp0, obp1) `shouldBe` (0x00, 0x00, 0x00)

        it "starts with APU off (NR52 bit 7 clear) so boot ROM controls power-on" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridgeWithBoot (Just trivialBootStub) cart
            nr52 <- Bus.read8 0xFF26 (Machine.machineBus m)
            (nr52 .&. 0x80) `shouldBe` 0x00

    describe "Machine constructed without a boot ROM (post-boot shortcut)" $ do
        it "still starts with LCDC=0x91 (regression guard)" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridge cart
            lcdc <- Bus.read8 0xFF40 (Machine.machineBus m)
            lcdc `shouldBe` 0x91

        it "still starts with BGP=0xFC (regression guard)" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridge cart
            bgp <- Bus.read8 0xFF47 (Machine.machineBus m)
            bgp `shouldBe` 0xFC

        it "still starts with APU on (NR52 bit 7 set, regression guard)" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridge cart
            nr52 <- Bus.read8 0xFF26 (Machine.machineBus m)
            (nr52 .&. 0x80) `shouldBe` 0x80
