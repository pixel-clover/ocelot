{-# LANGUAGE OverloadedStrings #-}

{- | CGB foundation: VRAM/WRAM banking and CGB palette register I/O.

These tests don't yet cover CGB-specific rendering (tile attributes,
RGB555 palettes feeding the framebuffer); they verify only the bus and
PPU register plumbing for VBK, BCPS\/BCPD, OCPS\/OCPD, WBK, and KEY1.
-}
module Ocelot.CgbSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import qualified Ocelot.Cpu.Registers as CR
import qualified Ocelot.Cpu.State as CS
import qualified Ocelot.Machine as Machine
import qualified Ocelot.Ppu as Ppu
import Test.Hspec

mkCgbRom :: BS.ByteString
mkCgbRom = mkRomWith 0x80 -- DmgAndCgb

mkDmgRom :: BS.ByteString
mkDmgRom = mkRomWith 0x00

mkRomWith :: Word8 -> BS.ByteString
mkRomWith cgbFlag =
    let romSize = 32 * 1024
        v0 = V.replicate romSize 0 :: V.Vector Word8
        title = "CGB"
        titleBytes =
            zip
                [0x0134 ..]
                (BS.unpack (BS.take 16 (BSC.pack title `BS.append` BS.replicate 16 0)))
        fields =
            [ (0x0100, 0x00)
            , (0x0101, 0xC3)
            , (0x0102, 0x50)
            , (0x0103, 0x01)
            , (0x0146, 0x00)
            , (0x0147, 0x00)
            , (0x0148, 0x00)
            , (0x0149, 0x00)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        -- CGB flag goes last so it wins over the title byte at 0x143.
        v1 = v0 V.// fields V.// [(0x0143, cgbFlag)]
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
     in BS.take 0x14D body0 <> BS.singleton cs <> BS.drop 0x14E body0

mkBus :: BS.ByteString -> IO Bus.Bus
mkBus rom = do
    Right cart <- Cartridge.loadRom rom
    Bus.fromCartridge cart

-- | Force a CGB host (the SDL frontend's choice for compat-mode tests),
-- even when the cart is DMG-only. Used by tests that exercise the
-- DMG-on-CGB auto-palette pipeline.
mkBusOnCgbHost :: BS.ByteString -> IO Bus.Bus
mkBusOnCgbHost rom = do
    Right cart <- Cartridge.loadRom rom
    Bus.fromCartridgeOnHost Bus.HostCgb cart

spec :: Spec
spec = do
    describe "CGB cartridge detection" $ do
        it "DmgAndCgb cart sets busCgb" $ do
            b <- mkBus mkCgbRom
            Bus.busCgb b `shouldBe` True

        it "DmgOnly cart leaves busCgb False" $ do
            b <- mkBus mkDmgRom
            Bus.busCgb b `shouldBe` False

    describe "DMG host gates CGB-only registers" $ do
        -- All addresses in this set are CGB-only registers; reads on a
        -- DMG host (the default for a DMG cart) must return 0xFF and
        -- writes are ignored.
        it "FF4D (KEY1), FF4F (VBK), FF55 (HDMA5), FF68/69/6A/6B (palettes), FF70 (SVBK) read 0xFF" $ do
            b <- mkBus mkDmgRom
            mapM_
                ( \addr -> do
                    -- Stuff the register first to make sure the gate is
                    -- intercepting reads, not just observing zeros.
                    Bus.write8 addr 0x42 b
                    v <- Bus.read8 addr b
                    (addr, v) `shouldBe` (addr, 0xFF)
                )
                [0xFF4D, 0xFF4F, 0xFF55, 0xFF68, 0xFF69, 0xFF6A, 0xFF6B, 0xFF70]

        it "unmapped I/O page addresses read 0xFF (mooneye unused_hwio)" $ do
            b <- mkBus mkDmgRom
            mapM_
                ( \addr -> do
                    Bus.write8 addr 0x42 b
                    v <- Bus.read8 addr b
                    (addr, v) `shouldBe` (addr, 0xFF)
                )
                [0xFF03, 0xFF08, 0xFF0E, 0xFF4C, 0xFF4E, 0xFF56, 0xFF6C, 0xFF7F]

        it "CGB cart starts the CPU at A=0x11 (post-boot platform probe)" $ do
            Right cart <- Cartridge.loadRom mkCgbRom
            m <- Machine.machineFromCartridge cart
            cpu <- readIORef (Machine.machineCpu m)
            CR.regA (CS.cpuRegs cpu) `shouldBe` 0x11

        it "DMG cart starts the CPU at A=0x01" $ do
            Right cart <- Cartridge.loadRom mkDmgRom
            m <- Machine.machineFromCartridge cart
            cpu <- readIORef (Machine.machineCpu m)
            CR.regA (CS.cpuRegs cpu) `shouldBe` 0x01

    describe "VRAM banking via VBK (0xFF4F)" $ do
        it "switches the active 8 KiB bank for 0x8000-0x9FFF" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4F 0x00 b
            Bus.write8 0x8000 0xAA b
            Bus.write8 0xFF4F 0x01 b
            Bus.write8 0x8000 0xBB b
            -- Reads route through the active bank.
            Bus.write8 0xFF4F 0x00 b
            v0 <- Bus.read8 0x8000 b
            Bus.write8 0xFF4F 0x01 b
            v1 <- Bus.read8 0x8000 b
            v0 `shouldBe` 0xAA
            v1 `shouldBe` 0xBB

        it "VBK reads back as the bank bit OR 0xFE" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4F 0x01 b
            v <- Bus.read8 0xFF4F b
            v `shouldBe` 0xFF
            Bus.write8 0xFF4F 0x00 b
            v0 <- Bus.read8 0xFF4F b
            v0 `shouldBe` 0xFE

    describe "WRAM banking via WBK (0xFF70)" $ do
        it "lower 4 KiB always maps to bank 0; upper 4 KiB switches" $ do
            b <- mkBus mkCgbRom
            -- Write a marker at 0xC000 (lower bank, never switches).
            Bus.write8 0xC000 0x11 b
            -- Bank 1 (default): write a different byte at 0xD000.
            Bus.write8 0xFF70 0x01 b
            Bus.write8 0xD000 0xA1 b
            -- Bank 2: distinct byte.
            Bus.write8 0xFF70 0x02 b
            Bus.write8 0xD000 0xA2 b
            -- Lower bank still has its marker regardless of WBK.
            v0 <- Bus.read8 0xC000 b
            v0 `shouldBe` 0x11
            -- Switching back to bank 1 sees A1, bank 2 sees A2.
            Bus.write8 0xFF70 0x01 b
            v1 <- Bus.read8 0xD000 b
            Bus.write8 0xFF70 0x02 b
            v2 <- Bus.read8 0xD000 b
            v1 `shouldBe` 0xA1
            v2 `shouldBe` 0xA2

        it "WBK = 0 acts as bank 1" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF70 0x01 b
            Bus.write8 0xD000 0x77 b
            Bus.write8 0xFF70 0x00 b
            v <- Bus.read8 0xD000 b
            v `shouldBe` 0x77

        it "WBK is ignored on a DMG-only cart" $ do
            b <- mkBus mkDmgRom
            Bus.write8 0xFF70 0x05 b
            v <- Bus.read8 0xFF70 b
            v `shouldBe` 0xFF

    describe "CGB BG palette RAM (0xFF68 / 0xFF69)" $ do
        it "BCPD writes land in palette RAM at the BCPS index" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0x05 b
            Bus.write8 0xFF69 0x42 b
            Bus.write8 0xFF68 0x05 b
            v <- Bus.read8 0xFF69 b
            v `shouldBe` 0x42

        it "BCPS auto-increment advances on write when bit 7 is set" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0x80 b -- index 0, auto-increment
            Bus.write8 0xFF69 0x11 b
            Bus.write8 0xFF69 0x22 b
            Bus.write8 0xFF69 0x33 b
            -- Index should now be 0x83 (auto-inc bit kept, low 6 bits = 3).
            ix <- Bus.read8 0xFF68 b
            ix `shouldBe` 0x83
            -- Walk the entries back at offsets 0..2.
            Bus.write8 0xFF68 0x00 b
            v0 <- Bus.read8 0xFF69 b
            Bus.write8 0xFF68 0x01 b
            v1 <- Bus.read8 0xFF69 b
            Bus.write8 0xFF68 0x02 b
            v2 <- Bus.read8 0xFF69 b
            (v0, v1, v2) `shouldBe` (0x11, 0x22, 0x33)

        it "auto-increment wraps at offset 0x3F" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF68 0xBF b -- index 0x3F, auto-increment
            Bus.write8 0xFF69 0xFE b
            ix <- Bus.read8 0xFF68 b
            ix `shouldBe` 0x80 -- wrapped to 0, auto-inc preserved
    describe "CGB OBJ palette RAM (0xFF6A / 0xFF6B)" $ do
        it "OCPD writes land in OBJ palette RAM" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF6A 0x80 b
            Bus.write8 0xFF6B 0xAA b
            Bus.write8 0xFF6B 0xBB b
            Bus.write8 0xFF6A 0x00 b
            v0 <- Bus.read8 0xFF6B b
            Bus.write8 0xFF6A 0x01 b
            v1 <- Bus.read8 0xFF6B b
            v0 `shouldBe` 0xAA
            v1 `shouldBe` 0xBB

    describe "KEY1 (0xFF4D) and double-speed" $ do
        it "round-trips bit 0; bit 7 reads as 0 before any STOP" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4D 0x01 b
            v <- Bus.read8 0xFF4D b
            v `shouldBe` 0x7F -- bit 0 set + bits 1..6 read as 1, bit 7 = 0
        it "is 0xFF on a DMG-only cart" $ do
            b <- mkBus mkDmgRom
            v <- Bus.read8 0xFF4D b
            v `shouldBe` 0xFF

        it "triggerSpeedSwitch flips bit 7 and clears prepare bit" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4D 0x01 b
            switched <- Bus.triggerSpeedSwitch b
            switched `shouldBe` True
            v <- Bus.read8 0xFF4D b
            -- bit 7 = 1 (double-speed), bit 0 = 0, others read as 1.
            v `shouldBe` 0xFE

        it "ignores triggerSpeedSwitch when prepare bit is clear" $ do
            b <- mkBus mkCgbRom
            switched <- Bus.triggerSpeedSwitch b
            switched `shouldBe` False
            v <- Bus.read8 0xFF4D b
            v `shouldBe` 0x7E

        it "in double-speed, peripherals tick at half the M-cycle rate" $ do
            -- In single-speed, 20 M-cycles = 80 T-cycles is exactly the
            -- Mode 2 -> Mode 3 boundary. In double-speed, the PPU sees
            -- only half the M-cycles passed to Bus.advance, so we need
            -- to advance 40 M-cycles to hit the same boundary.
            b <- mkBus mkCgbRom
            Bus.write8 0xFF4D 0x01 b
            _ <- Bus.triggerSpeedSwitch b
            let ps = Bus.busPpu b
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            -- 20 M-cycles in double-speed = 10 PPU M-cycles = 40 T-cycles;
            -- still in Mode 2.
            Bus.advance 20 b
            mode <- readIORef (Ppu.ppuMode ps)
            mode `shouldBe` Ppu.ModeOamScan
            -- 20 more M-cycles cross into Mode 3.
            Bus.advance 20 b
            mode2 <- readIORef (Ppu.ppuMode ps)
            mode2 `shouldBe` Ppu.ModeDrawing

    describe "CGB BG rendering" $ do
        it "draws BG palette 0 colors when the attribute byte selects palette 0" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Tile 0 row 0: striped (color indices 0,1,0,1,...)
            -- Bytes: low=0x55, high=0x00 -> bits select color 1 every other px.
            MV.write (Ppu.ppuVram ps) 0 0x55
            MV.write (Ppu.ppuVram ps) 1 0x00
            -- Tilemap entry at 0x9800 = tile 0 (already 0).
            -- Bank 1 attribute at the same offset = palette 0, no flips, bank 0.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x00
            -- Palette 0 color 0 = pure red (RGB555 = 0x001F), color 1 = pure
            -- green (RGB555 = 0x03E0). Encoded little-endian.
            mapM_
                (uncurry (MV.write (Ppu.ppuBgPalRam ps)))
                [(0, 0x1F), (1, 0x00), (2, 0xE0), (3, 0x03)]
            -- LCDC: enable LCD + BG; BG tile data unsigned mode; 8x8 sprites.
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            -- Run one full scanline through OAM scan + drawing + HBlank.
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Pixel 0 was bit-7 of (low=0x55,high=0x00) = 0; color 0 -> red.
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0xFF, 0x00, 0x00)
            -- Pixel 1 bit-6: low=1, high=0 -> color 1 -> green.
            (rgb V.! 3, rgb V.! 4, rgb V.! 5) `shouldBe` (0x00, 0xFF, 0x00)

        it "DMG cart on CGB host renders through the compat auto-palette" $ do
            -- Forced to a CGB host so a DMG-only cart selects
            -- RenderCgbCompat. The default (no-title-match) auto-palette
            -- is grayscale; shade 1 decodes to RGB (0x94, 0x94, 0x94).
            -- Default 'mkBus' would now pick a DMG host for a DMG cart.
            b <- mkBusOnCgbHost mkDmgRom
            let ps = Bus.busPpu b
            MV.write (Ppu.ppuVram ps) 0 0xFF
            MV.write (Ppu.ppuVram ps) 1 0x00
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x94, 0x94, 0x94)

        it "forcing RenderDmg gives the hardcoded greenish-DMG palette" $ do
            -- Same setup as above, but explicitly switch the render mode
            -- to RenderDmg (what a pure DMG host would do): shade 1 ->
            -- (0x88, 0xC0, 0x70) per the DMG shade ramp.
            b <- mkBus mkDmgRom
            let ps = Bus.busPpu b
            Ppu.setCgbRenderMode Ppu.RenderDmg ps
            MV.write (Ppu.ppuVram ps) 0 0xFF
            MV.write (Ppu.ppuVram ps) 1 0x00
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x88, 0xC0, 0x70)

        it "respects the CGB tile-data bank (attribute bit 3)" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Bank 0 tile 0: all zeros (color 0 everywhere).
            -- Bank 1 tile 0: 0xFF/0x00 -> color 1 across the whole row.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0) 0xFF
            MV.write (Ppu.ppuVram ps) (0x2000 + 1) 0x00
            -- Tilemap entry: tile 0; attr selects palette 0, bank 1 (bit 3 set).
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x08
            mapM_
                (uncurry (MV.write (Ppu.ppuBgPalRam ps)))
                [(0, 0x00), (1, 0x00), (2, 0xE0), (3, 0x03)]
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x00, 0xFF, 0x00)

        it "draws CGB sprites through OBJ palette RAM" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Sprite tile 1 (bank 0): all bits set in low byte (color 1
            -- across the row). Tile data at 0x10..0x1F.
            MV.write (Ppu.ppuVram ps) 0x10 0xFF
            MV.write (Ppu.ppuVram ps) 0x11 0x00
            -- Sprite at OAM 0: y=16, x=8 (so it shows at line 0, pixel 0..7).
            -- attr = 0x02 (palette 2, no flip, bank 0, low priority).
            MV.write (Ppu.ppuOam ps) 0 16
            MV.write (Ppu.ppuOam ps) 1 8
            MV.write (Ppu.ppuOam ps) 2 0x01
            MV.write (Ppu.ppuOam ps) 3 0x02
            -- OBJ palette 2 color 1 at offset 2*8 + 1*2 = 18..19.
            -- Set to pure blue (RGB555 = 0x7C00).
            MV.write (Ppu.ppuObjPalRam ps) 18 0x00
            MV.write (Ppu.ppuObjPalRam ps) 19 0x7C
            -- LCDC: BG + sprites enabled, 8x8 sprites.
            writeIORef (Ppu.ppuLcdc ps) 0x93
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Pixel 0..7 should be blue (sprite covers BG color 0).
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x00, 0x00, 0xFF)

        it "respects CGB sprite tile bank (OAM attr bit 3)" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Bank 0 tile 1: empty (would render as transparent color 0).
            -- Bank 1 tile 1: 0xFF/0x00 -> color 1 across the row.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x10) 0xFF
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x11) 0x00
            MV.write (Ppu.ppuOam ps) 0 16
            MV.write (Ppu.ppuOam ps) 1 8
            MV.write (Ppu.ppuOam ps) 2 0x01
            -- attr = 0x08: palette 0, bank 1.
            MV.write (Ppu.ppuOam ps) 3 0x08
            -- OBJ palette 0 color 1 = pure red.
            MV.write (Ppu.ppuObjPalRam ps) 2 0x1F
            MV.write (Ppu.ppuObjPalRam ps) 3 0x00
            writeIORef (Ppu.ppuLcdc ps) 0x93
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0xFF, 0x00, 0x00)

        it "HDMA general-mode copies the full payload immediately" $ do
            b <- mkBus mkCgbRom
            -- Plant 16 source bytes in WRAM at 0xC000..0xC00F.
            mapM_ (\i -> Bus.write8 (0xC000 + fromIntegral i) (fromIntegral i) b) [0 .. 15 :: Int]
            -- Source = 0xC000, dest = 0x9000, length = 16 (HDMA5 lo = 0).
            Bus.write8 0xFF51 0xC0 b
            Bus.write8 0xFF52 0x00 b
            Bus.write8 0xFF53 0x10 b -- dest hi: 0x9000 = 0x8000 | (0x10 << 8)
            Bus.write8 0xFF54 0x00 b
            Bus.write8 0xFF55 0x00 b -- start general DMA, length = 1 chunk
            -- Read back via VRAM.
            vs <- mapM (\i -> Bus.read8 (0x9000 + fromIntegral i) b) [0 .. 15 :: Int]
            vs `shouldBe` [0 .. 15]
            -- HDMA5 reads as 0xFF when idle.
            v55 <- Bus.read8 0xFF55 b
            v55 `shouldBe` 0xFF

        it "HDMA general-mode advances peripherals during the copy block" $ do
            -- Regression: general DMA used to be instant. The fix advances
            -- peripherals for length / 2 M-cycles in single-speed so the
            -- PPU continues to tick instead of jumping forward only on the
            -- next instruction. Verify by checking the divider moves.
            b <- mkBus mkCgbRom
            let initialDiv = 0
            -- 4 chunks = 64 bytes; in single-speed that's 32 M-cycles of
            -- block time, which advances the timer's internal counter by
            -- 128 T-cycles (32 * 4).
            Bus.write8 0xFF51 0xC0 b
            Bus.write8 0xFF52 0x00 b
            Bus.write8 0xFF53 0x10 b
            Bus.write8 0xFF54 0x00 b
            Bus.write8 0xFF55 0x03 b -- general DMA, 4 chunks
            divAfter <- Bus.read8 0xFF04 b
            -- DIV exposes bits 8..15 of the 16-bit counter; after 128
            -- T-cycles the counter is 128, so DIV is still 0. Push more
            -- via a larger transfer to confirm the counter actually moves.
            Bus.write8 0xFF51 0xC0 b
            Bus.write8 0xFF52 0x00 b
            Bus.write8 0xFF53 0x10 b
            Bus.write8 0xFF54 0x00 b
            Bus.write8 0xFF55 0x7F b -- 128 chunks = 2048 bytes -> 1024 M-cycles -> 4096 T-cycles
            divAfter2 <- Bus.read8 0xFF04 b
            divAfter `shouldBe` initialDiv -- still 0
            -- 4096 T-cycles + 128 prior = 4224 = 0x1080 -> upper byte 0x10.
            divAfter2 `shouldBe` 0x10

        it "HDMA HBlank-mode copies one chunk per HBlank entry" $ do
            b <- mkBus mkCgbRom
            -- 32 bytes of source pattern starting at 0xC000.
            mapM_ (\i -> Bus.write8 (0xC000 + fromIntegral i) (fromIntegral i + 0x10) b) [0 .. 31 :: Int]
            Bus.write8 0xFF51 0xC0 b
            Bus.write8 0xFF52 0x00 b
            Bus.write8 0xFF53 0x10 b
            Bus.write8 0xFF54 0x00 b
            -- Start HBlank DMA, 2 chunks: HDMA5 = 0x80 | (n-1) = 0x81.
            Bus.write8 0xFF55 0x81 b
            -- LCD on, in OamScan, line 0; we will hit HBlank as we advance.
            let ps = Bus.busPpu b
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            -- Step exactly one full scanline -> one HBlank entry -> one chunk.
            Bus.advance 114 b
            chunk1 <- mapM (\i -> Bus.read8 (0x9000 + fromIntegral i) b) [0 .. 15 :: Int]
            chunk1 `shouldBe` map (+ 0x10) [0 .. 15]
            -- Second chunk should still be empty (default 0 in VRAM).
            beforeSecond <- Bus.read8 0x9010 b
            beforeSecond `shouldBe` 0x00
            -- HDMA5 should report bit 7 = 0 (active) and remaining = 1 chunk.
            mid <- Bus.read8 0xFF55 b
            mid `shouldBe` 0x00
            -- Step another scanline -> second chunk lands.
            Bus.advance 114 b
            chunk2 <- mapM (\i -> Bus.read8 (0x9010 + fromIntegral i) b) [0 .. 15 :: Int]
            chunk2 `shouldBe` map (+ 0x10) [16 .. 31]
            done <- Bus.read8 0xFF55 b
            done `shouldBe` 0xFF

        it "CGB sprite priority follows OAM order, not X position" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Both sprites overlap pixel 0..7 at line 0.
            -- Sprite 0 (lower OAM index → higher priority): tile 1 = red.
            MV.write (Ppu.ppuVram ps) 0x10 0xFF
            MV.write (Ppu.ppuVram ps) 0x11 0x00
            MV.write (Ppu.ppuOam ps) 0 16
            MV.write (Ppu.ppuOam ps) 1 8 -- x=8 (rightmost in DMG terms)
            MV.write (Ppu.ppuOam ps) 2 0x01
            MV.write (Ppu.ppuOam ps) 3 0x00 -- palette 0
            -- Sprite 1: tile 2 = blue, but at x=8 (would have higher
            -- priority on DMG by sort-by-X if X were lower; same X
            -- here so OAM index alone matters).
            MV.write (Ppu.ppuVram ps) 0x20 0xFF
            MV.write (Ppu.ppuVram ps) 0x21 0x00
            MV.write (Ppu.ppuOam ps) 4 16
            MV.write (Ppu.ppuOam ps) 5 8
            MV.write (Ppu.ppuOam ps) 6 0x02
            MV.write (Ppu.ppuOam ps) 7 0x01 -- palette 1
            -- OBJ palette 0 color 1 = red, OBJ palette 1 color 1 = blue.
            mapM_
                (uncurry (MV.write (Ppu.ppuObjPalRam ps)))
                [ (2, 0x1F)
                , (3, 0x00) -- pal 0 col 1 = red
                , (10, 0x00)
                , (11, 0x7C) -- pal 1 col 1 = blue
                ]
            writeIORef (Ppu.ppuLcdc ps) 0x93
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Sprite 0 (OAM index 0) wins → red.
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0xFF, 0x00, 0x00)

        it "BG attr bit 7 keeps BG in front of a sprite over BG color 1" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- BG tile 0: bit row low=0xFF -> color 1 across the row.
            MV.write (Ppu.ppuVram ps) 0 0xFF
            MV.write (Ppu.ppuVram ps) 1 0x00
            -- BG attr bit 7 = priority over OBJ; palette 0.
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x80
            -- Sprite tile 1, bank 0: also color 1 across the row.
            MV.write (Ppu.ppuVram ps) 0x10 0xFF
            MV.write (Ppu.ppuVram ps) 0x11 0x00
            MV.write (Ppu.ppuOam ps) 0 16
            MV.write (Ppu.ppuOam ps) 1 8
            MV.write (Ppu.ppuOam ps) 2 0x01
            MV.write (Ppu.ppuOam ps) 3 0x00
            -- BG palette 0 color 1 = pure red, OBJ palette 0 color 1 = pure blue.
            mapM_
                (uncurry (MV.write (Ppu.ppuBgPalRam ps)))
                [(0, 0xFF), (1, 0xFF), (2, 0x1F), (3, 0x00)]
            mapM_
                (uncurry (MV.write (Ppu.ppuObjPalRam ps)))
                [(0, 0xFF), (1, 0xFF), (2, 0x00), (3, 0x7C)]
            -- LCDC: master priority on (bit 0) + BG + sprites.
            writeIORef (Ppu.ppuLcdc ps) 0x93
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- BG should win at pixel 0..7 thanks to BG attr bit 7 (red, not blue).
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0xFF, 0x00, 0x00)

        it "LCDC bit 0 = 0 forces OBJ on top of BG (CGB master priority)" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Same setup as above, but with master priority off.
            MV.write (Ppu.ppuVram ps) 0 0xFF
            MV.write (Ppu.ppuVram ps) 1 0x00
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x80 -- BG priority bit set
            MV.write (Ppu.ppuVram ps) 0x10 0xFF
            MV.write (Ppu.ppuVram ps) 0x11 0x00
            MV.write (Ppu.ppuOam ps) 0 16
            MV.write (Ppu.ppuOam ps) 1 8
            MV.write (Ppu.ppuOam ps) 2 0x01
            MV.write (Ppu.ppuOam ps) 3 0x00
            mapM_
                (uncurry (MV.write (Ppu.ppuBgPalRam ps)))
                [(0, 0xFF), (1, 0xFF), (2, 0x1F), (3, 0x00)]
            mapM_
                (uncurry (MV.write (Ppu.ppuObjPalRam ps)))
                [(0, 0xFF), (1, 0xFF), (2, 0x00), (3, 0x7C)]
            -- LCDC bit 0 = 0 (master priority off): sprites win regardless.
            writeIORef (Ppu.ppuLcdc ps) 0x92
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- OBJ should win → blue.
            (rgb V.! 0, rgb V.! 1, rgb V.! 2) `shouldBe` (0x00, 0x00, 0xFF)

        it "writing to HDMA5 with bit 7 = 0 cancels an active HBlank transfer" $ do
            b <- mkBus mkCgbRom
            Bus.write8 0xFF51 0xC0 b
            Bus.write8 0xFF52 0x00 b
            Bus.write8 0xFF53 0x10 b
            Bus.write8 0xFF54 0x00 b
            Bus.write8 0xFF55 0x83 b -- HBlank DMA, 4 chunks
            mid <- Bus.read8 0xFF55 b
            mid `shouldBe` 0x03 -- bit 7 = 0 (active), remaining = 4-1
            -- Cancel by writing bit 7 = 0.
            Bus.write8 0xFF55 0x00 b
            after <- Bus.read8 0xFF55 b
            after `shouldBe` 0xFF -- inactive
        it "horizontal-flip attribute reverses pixel order" $ do
            b <- mkBus mkCgbRom
            let ps = Bus.busPpu b
            -- Tile row: low=0x80 (only bit 7 set) -> color 1 only at pixel 0.
            MV.write (Ppu.ppuVram ps) 0 0x80
            MV.write (Ppu.ppuVram ps) 1 0x00
            -- Attribute: palette 0, hflip set (bit 5).
            MV.write (Ppu.ppuVram ps) (0x2000 + 0x1800) 0x20
            -- Palette 0: color 0 = white, color 1 = red.
            mapM_
                (uncurry (MV.write (Ppu.ppuBgPalRam ps)))
                [(0, 0xFF), (1, 0x7F), (2, 0x1F), (3, 0x00)]
            writeIORef (Ppu.ppuLcdc ps) 0x91
            writeIORef (Ppu.ppuBgp ps) 0xE4
            writeIORef (Ppu.ppuMode ps) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ps) 0
            writeIORef (Ppu.ppuLy ps) 0
            _ <- Ppu.advance 114 (Bus.busPpu b)
            rgb <- Ppu.framebufferRgb (Bus.busPpu b)
            -- Without hflip the red pixel would be at pixel 0; with hflip
            -- it should be at pixel 7.
            let pixelRgb i = (rgb V.! (i * 3), rgb V.! (i * 3 + 1), rgb V.! (i * 3 + 2))
            pixelRgb 0 `shouldBe` (0xFF, 0xFF, 0xFF)
            pixelRgb 7 `shouldBe` (0xFF, 0x00, 0x00)
