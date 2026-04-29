{-# LANGUAGE OverloadedStrings #-}

module Ocelot.SnapshotSpec (spec) where

import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cart
import Ocelot.Cpu.Registers (Registers (..))
import Ocelot.Cpu.State (CpuState (..))
import qualified Ocelot.Joypad as Joypad
import Ocelot.Machine (Machine (..), machineFromCartridge)
import qualified Ocelot.Machine
import qualified Ocelot.Ppu as Ppu
import qualified Ocelot.Snapshot as Snap
import Ocelot.Timer (TimerState (..))
import Test.Hspec

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as V
import Ocelot.Cartridge.Header (expectedHeaderChecksum)

mkRom :: BS.ByteString
mkRom =
    let romSize = 32 * 1024
        v0 = V.replicate romSize 0 :: V.Vector Word8
        title = "SNAPTEST"
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
            , (0x0147, 0x13) -- MBC3 + RAM + battery
            , (0x0148, 0x00)
            , (0x0149, 0x02)
            , (0x014A, 0x00)
            , (0x014B, 0x33)
            , (0x014C, 0x00)
            ]
                <> titleBytes
        v1 = v0 V.// fields
        body0 = BS.pack (V.toList v1)
        cs = expectedHeaderChecksum body0
        body1 = BS.take 0x14D body0 <> BS.singleton cs <> BS.drop 0x14E body0
     in body1

mkMachine :: IO Machine
mkMachine = do
    Right cart <- Cart.loadRom mkRom
    machineFromCartridge cart

spec :: Spec
spec = do
    describe "Snapshot.save / load" $ do
        it "round-trips CPU registers and IME flag" $ do
            m <- mkMachine
            writeIORef
                (machineCpu m)
                CpuState
                    { cpuRegs = Registers 0x12 0xA0 0x34 0x56 0x78 0x9A 0xBC 0xDE 0x1234 0x5678
                    , cpuIme = True
                    , cpuEiDelay = False
                    , cpuHalted = False
                    , cpuCycles = 0xCAFEBABE
                    }
            blob <- Snap.save m
            -- Mutate before reload to be sure load actually overwrites.
            writeIORef
                (machineCpu m)
                CpuState
                    { cpuRegs = Registers 0 0 0 0 0 0 0 0 0 0
                    , cpuIme = False
                    , cpuEiDelay = False
                    , cpuHalted = False
                    , cpuCycles = 0
                    }
            r <- Snap.load blob m
            r `shouldBe` Right ()
            cpu <- readIORef (machineCpu m)
            regA (cpuRegs cpu) `shouldBe` 0x12
            regPC (cpuRegs cpu) `shouldBe` 0x5678
            cpuIme cpu `shouldBe` True
            cpuCycles cpu `shouldBe` 0xCAFEBABE

        it "round-trips Timer state" $ do
            m <- mkMachine
            writeIORef
                (Bus.busTimer (machineBus m))
                TimerState
                    { timDivider = 0xABCD
                    , timTima = 0x55
                    , timTma = 0x66
                    , timTac = 0x07
                    , timPrevAnd = True
                    , timReloadCounter = 3
                    }
            blob <- Snap.save m
            writeIORef (Bus.busTimer (machineBus m)) (TimerState 0 0 0 0 False 0)
            _ <- Snap.load blob m
            ts <- readIORef (Bus.busTimer (machineBus m))
            ts
                `shouldBe` TimerState
                    { timDivider = 0xABCD
                    , timTima = 0x55
                    , timTma = 0x66
                    , timTac = 0x07
                    , timPrevAnd = True
                    , timReloadCounter = 3
                    }

        it "round-trips PPU regs and VRAM" $ do
            m <- mkMachine
            let ppu = Bus.busPpu (machineBus m)
            writeIORef (Ppu.ppuLcdc ppu) 0x91
            writeIORef (Ppu.ppuLy ppu) 0x42
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeDrawing
            writeIORef (Ppu.ppuDot ppu) 123
            MV.write (Ppu.ppuVram ppu) 0x100 0xAB
            blob <- Snap.save m
            -- Clobber.
            writeIORef (Ppu.ppuLcdc ppu) 0x00
            writeIORef (Ppu.ppuLy ppu) 0x00
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeOamScan
            writeIORef (Ppu.ppuDot ppu) 0
            MV.write (Ppu.ppuVram ppu) 0x100 0x00
            _ <- Snap.load blob m
            lcdc <- readIORef (Ppu.ppuLcdc ppu)
            ly <- readIORef (Ppu.ppuLy ppu)
            mode <- readIORef (Ppu.ppuMode ppu)
            dot <- readIORef (Ppu.ppuDot ppu)
            v <- MV.read (Ppu.ppuVram ppu) 0x100
            lcdc `shouldBe` 0x91
            ly `shouldBe` 0x42
            mode `shouldBe` Ppu.ModeDrawing
            dot `shouldBe` 123
            v `shouldBe` 0xAB

        it "round-trips WRAM and IE" $ do
            m <- mkMachine
            let bus = machineBus m
            MV.write (Bus.busWram bus) 0x1000 0x77
            writeIORef (Bus.busIe bus) 0x1F
            blob <- Snap.save m
            MV.write (Bus.busWram bus) 0x1000 0x00
            writeIORef (Bus.busIe bus) 0x00
            _ <- Snap.load blob m
            v <- MV.read (Bus.busWram bus) 0x1000
            ie <- readIORef (Bus.busIe bus)
            v `shouldBe` 0x77
            ie `shouldBe` 0x1F

        it "round-trips Joypad state" $ do
            m <- mkMachine
            let jp = Bus.busJoypad (machineBus m)
            Joypad.setButton Joypad.ButtonA True jp
            Joypad.setButton Joypad.ButtonStart True jp
            Joypad.writeP1 0x10 jp -- select action row
            blob <- Snap.save m
            Joypad.setButton Joypad.ButtonA False jp
            Joypad.setButton Joypad.ButtonStart False jp
            Joypad.writeP1 0x00 jp
            _ <- Snap.load blob m
            a <- Joypad.isPressed Joypad.ButtonA jp
            s <- Joypad.isPressed Joypad.ButtonStart jp
            d <- Joypad.isPressed Joypad.ButtonDown jp
            a `shouldBe` True
            s `shouldBe` True
            d `shouldBe` False

        it "round-trips Cart RAM and MBC bank state" $ do
            m <- mkMachine
            let cart = Bus.busCart (machineBus m)
            -- Enable RAM and write a byte; bump ROM bank.
            Cart.write8 0x0000 0x0A cart
            Cart.write8 0x4000 0x00 cart
            Cart.write8 0xA000 0xEE cart
            Cart.write8 0x2000 0x05 cart -- bank 5
            blob <- Snap.save m
            -- Clobber: pick a different bank, overwrite RAM.
            Cart.write8 0x2000 0x01 cart
            Cart.write8 0xA000 0x00 cart
            _ <- Snap.load blob m
            -- After load, we should be back on bank 5 and RAM has 0xEE.
            v <- Cart.read8 0xA000 cart
            v `shouldBe` 0xEE

        it "rejects a blob with the wrong magic" $ do
            m <- mkMachine
            r <- Snap.load (BS.replicate 64 0) m
            r `shouldBe` Left Snap.BadMagic

        it "round-trips CGB-only state (VBK, palette RAM, WBK, KEY1)" $ do
            m <- mkMachine
            let bus = Ocelot.Machine.machineBus m
                ppu = Bus.busPpu bus
            -- Stage some non-default values.
            writeIORef (Ppu.ppuVbk ppu) 0x01
            writeIORef (Ppu.ppuBcps ppu) 0x82
            writeIORef (Ppu.ppuOcps ppu) 0x05
            MV.write (Ppu.ppuBgPalRam ppu) 0 0xAA
            MV.write (Ppu.ppuBgPalRam ppu) 63 0xCC
            MV.write (Ppu.ppuObjPalRam ppu) 1 0x55
            writeIORef (Bus.busWramBank bus) 0x05
            writeIORef (Bus.busKey1 bus) 0x01
            blob <- Snap.save m
            -- Clobber.
            writeIORef (Ppu.ppuVbk ppu) 0x00
            writeIORef (Ppu.ppuBcps ppu) 0x00
            writeIORef (Ppu.ppuOcps ppu) 0x00
            MV.write (Ppu.ppuBgPalRam ppu) 0 0x00
            MV.write (Ppu.ppuBgPalRam ppu) 63 0x00
            MV.write (Ppu.ppuObjPalRam ppu) 1 0x00
            writeIORef (Bus.busWramBank bus) 0x00
            writeIORef (Bus.busKey1 bus) 0x00
            r <- Snap.load blob m
            r `shouldBe` Right ()
            vbk <- readIORef (Ppu.ppuVbk ppu)
            bcps <- readIORef (Ppu.ppuBcps ppu)
            ocps <- readIORef (Ppu.ppuOcps ppu)
            bg0 <- MV.read (Ppu.ppuBgPalRam ppu) 0
            bg63 <- MV.read (Ppu.ppuBgPalRam ppu) 63
            obj1 <- MV.read (Ppu.ppuObjPalRam ppu) 1
            wbk <- readIORef (Bus.busWramBank bus)
            key1 <- readIORef (Bus.busKey1 bus)
            vbk `shouldBe` 0x01
            bcps `shouldBe` 0x82
            ocps `shouldBe` 0x05
            bg0 `shouldBe` 0xAA
            bg63 `shouldBe` 0xCC
            obj1 `shouldBe` 0x55
            wbk `shouldBe` 0x05
            key1 `shouldBe` 0x01

        it "round-trips HDMA and double-speed state (v3)" $ do
            m <- mkMachine
            let bus = Ocelot.Machine.machineBus m
            writeIORef (Bus.busHdmaSrc bus) 0xC100
            writeIORef (Bus.busHdmaDst bus) 0x9300
            writeIORef (Bus.busHdmaLen bus) 96
            writeIORef (Bus.busHdmaActive bus) True
            writeIORef (Bus.busDoubleSpeed bus) True
            writeIORef (Bus.busDoubleSpeedAcc bus) 1
            blob <- Snap.save m
            -- Clobber.
            writeIORef (Bus.busHdmaSrc bus) 0
            writeIORef (Bus.busHdmaDst bus) 0
            writeIORef (Bus.busHdmaLen bus) 0
            writeIORef (Bus.busHdmaActive bus) False
            writeIORef (Bus.busDoubleSpeed bus) False
            writeIORef (Bus.busDoubleSpeedAcc bus) 0
            r <- Snap.load blob m
            r `shouldBe` Right ()
            src <- readIORef (Bus.busHdmaSrc bus)
            dst <- readIORef (Bus.busHdmaDst bus)
            len <- readIORef (Bus.busHdmaLen bus)
            active <- readIORef (Bus.busHdmaActive bus)
            ds <- readIORef (Bus.busDoubleSpeed bus)
            dsAcc <- readIORef (Bus.busDoubleSpeedAcc bus)
            src `shouldBe` 0xC100
            dst `shouldBe` 0x9300
            len `shouldBe` 96
            active `shouldBe` True
            ds `shouldBe` True
            dsAcc `shouldBe` 1

        it "rejects a blob with an unknown version" $ do
            m <- mkMachine
            blob <- Snap.save m
            -- Swap version u32 (offset 4..7) to 99.
            let header = BS.take 4 blob
                rest = BS.drop 8 blob
                bumped = header <> BS.pack [99, 0, 0, 0] <> rest
            r <- Snap.load bumped m
            r `shouldBe` Left (Snap.UnsupportedVersion 99)
