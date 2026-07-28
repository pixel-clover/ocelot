{-# LANGUAGE OverloadedStrings #-}

module Ocelot.BusSpec (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.IORef (writeIORef)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Ocelot.Bus (Bus, advance, drainSerial, fromCartridge, installBootRom, read8, write8)
import qualified Ocelot.Bus as Bus
import Ocelot.Cartridge (loadRom)
import Ocelot.Cartridge.Header (expectedHeaderChecksum)
import Ocelot.Joypad (Button (..))
import qualified Ocelot.Ppu as Ppu
import Ocelot.Testing (synthNoMbcRom)
import Test.Hspec

emptyBus :: IO Bus
emptyBus = do
    let rom = synthNoMbcRom BS.empty
    result <- loadRom rom
    case result of
        Right c -> fromCartridge c
        Left e -> error ("test setup: cartridge did not load: " ++ show e)

cgbBus :: IO Bus
cgbBus = do
    result <- loadRom (synthCgbNoMbcRom BS.empty)
    case result of
        Right c -> fromCartridge c
        Left e -> error ("test setup: CGB cartridge did not load: " ++ show e)

synthCgbNoMbcRom :: BS.ByteString -> BS.ByteString
synthCgbNoMbcRom prog =
    let patchedFlag =
            replaceByte 0x0143 0x80 (synthNoMbcRom prog)
        checksum = expectedHeaderChecksum patchedFlag
     in replaceByte 0x014D checksum patchedFlag

replaceByte :: Int -> Word8 -> BS.ByteString -> BS.ByteString
replaceByte offset byte bs =
    BS.take offset bs <> BS.singleton byte <> BS.drop (offset + 1) bs

spec :: Spec
spec = do
    describe "address routing" $ do
        it "WRAM round-trips a byte at 0xC000" $ do
            b <- emptyBus
            write8 0xC000 0xAB b
            v <- read8 0xC000 b
            v `shouldBe` 0xAB

        it "echo RAM at 0xE000 mirrors WRAM at 0xC000" $ do
            b <- emptyBus
            write8 0xC000 0xCD b
            v <- read8 0xE000 b
            v `shouldBe` 0xCD

        it "echo writes at 0xE000 are visible at 0xC000" $ do
            b <- emptyBus
            write8 0xE000 0x42 b
            v <- read8 0xC000 b
            v `shouldBe` 0x42

        it "HRAM round-trips a byte at 0xFF80" $ do
            b <- emptyBus
            write8 0xFF80 0x33 b
            v <- read8 0xFF80 b
            v `shouldBe` 0x33

        it "OAM round-trips a byte at 0xFE00" $ do
            b <- emptyBus
            let ppu = Bus.busPpu b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeHBlank
            write8 0xFE00 0x77 b
            v <- read8 0xFE00 b
            v `shouldBe` 0x77

        it "IE byte at 0xFFFF round-trips" $ do
            b <- emptyBus
            write8 0xFFFF 0x1F b
            v <- read8 0xFFFF b
            v `shouldBe` 0x1F

        it "unusable region 0xFEA0..0xFEFF returns 0xFF" $ do
            b <- emptyBus
            v0 <- read8 0xFEA0 b
            v1 <- read8 0xFEFF b
            v0 `shouldBe` 0xFF
            v1 `shouldBe` 0xFF

        it "ROM writes are forwarded to the cartridge (NoMbc ignores them)" $ do
            b <- emptyBus
            write8 0x0000 0x55 b
            v <- read8 0x0000 b
            v `shouldBe` 0x00

    describe "PPU access gating" $ do
        it "blocks CPU VRAM reads and writes during mode 3" $ do
            b <- emptyBus
            let ppu = Bus.busPpu b
            writeIORef (Ppu.ppuLcdc ppu) 0x80
            write8 0x8000 0x12 b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeDrawing
            blocked <- read8 0x8000 b
            write8 0x8000 0x34 b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeHBlank
            visible <- read8 0x8000 b
            blocked `shouldBe` 0xFF
            visible `shouldBe` 0x12

        it "blocks CPU OAM reads and writes during mode 2 and mode 3" $ do
            b <- emptyBus
            let ppu = Bus.busPpu b
            writeIORef (Ppu.ppuLcdc ppu) 0x80
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeHBlank
            write8 0xFE00 0x55 b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeOamScan
            blockedMode2 <- read8 0xFE00 b
            write8 0xFE00 0x66 b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeDrawing
            blockedMode3 <- read8 0xFE00 b
            writeIORef (Ppu.ppuMode ppu) Ppu.ModeHBlank
            visible <- read8 0xFE00 b
            blockedMode2 `shouldBe` 0xFF
            blockedMode3 `shouldBe` 0xFF
            visible `shouldBe` 0x55

    describe "serial-port capture" $ do
        it "writing 0x81 to SC after staging SB latches a byte to drainSerial" $ do
            b <- emptyBus
            write8 0xFF01 0x68 b
            write8 0xFF02 0x81 b
            ser <- drainSerial b
            ser `shouldBe` [0x68]

        it "captures multiple bytes in order" $ do
            b <- emptyBus
            mapM_
                (\ch -> write8 0xFF01 ch b >> write8 0xFF02 0x81 b)
                [0x48, 0x49, 0x21]
            ser <- drainSerial b
            ser `shouldBe` [0x48, 0x49, 0x21]

        it "holds the SC start bit until the transfer completes" $ do
            b <- emptyBus
            write8 0xFF01 0x41 b
            write8 0xFF02 0x81 b
            during <- read8 0xFF02 b
            -- An internal-clock transfer shifts 8 bits at 8192 Hz, i.e. 512
            -- T-cycles = 128 M-cycles. Until then SC bit 7 stays set.
            (during .&. 0x80) `shouldBe` 0x80
            advance 127 b
            justBefore <- read8 0xFF02 b
            (justBefore .&. 0x80) `shouldBe` 0x80
            advance 1 b
            after <- read8 0xFF02 b
            -- SC bits 6..1 read as 1 on hardware; bit 0 (internal clock) holds
            -- its written value, so the completed readback is 0x7F.
            (after .&. 0x80) `shouldBe` 0x00
            (after .&. 0x01) `shouldBe` 0x01

        it "raises IF bit 3 when the transfer completes" $ do
            b <- emptyBus
            write8 0xFF01 0x41 b
            write8 0xFF02 0x81 b
            during <- read8 0xFF0F b
            (during .&. 0x08) `shouldBe` 0x00
            advance 128 b
            after <- read8 0xFF0F b
            (after .&. 0x08) `shouldBe` 0x08

        it "reads back 0xFF from SB after a transfer with no link peer" $ do
            b <- emptyBus
            write8 0xFF01 0x41 b
            write8 0xFF02 0x81 b
            advance 128 b
            sb <- read8 0xFF01 b
            sb `shouldBe` 0xFF

        it "takes the same CPU M-cycle count in double-speed mode" $ do
            -- The serial shift clock hangs off the CPU clock, so like OAM DMA
            -- it sits on the CPU side of the speed divider: a transfer still
            -- costs 128 CPU M-cycles (half the wall-clock time) in double
            -- speed. Ticking it at the halved peripheral rate would stall a
            -- CGB game that busy-waits ~128 cycles for its transfer.
            b <- cgbBus
            write8 0xFF4D 0x01 b -- Arm the KEY1 speed switch
            switched <- Bus.triggerSpeedSwitch b
            switched `shouldBe` True
            write8 0xFF01 0x41 b
            write8 0xFF02 0x81 b
            advance 127 b
            mid <- read8 0xFF0F b
            (mid .&. 0x08) `shouldBe` 0x00
            advance 1 b
            after <- read8 0xFF0F b
            (after .&. 0x08) `shouldBe` 0x08

        it "an external-clock transfer never completes without a peer" $ do
            b <- emptyBus
            write8 0xFF01 0x41 b
            write8 0xFF02 0x80 b -- bit 7 set, bit 0 clear: external clock
            advance 4096 b
            sc <- read8 0xFF02 b
            iflag <- read8 0xFF0F b
            (sc .&. 0x80) `shouldBe` 0x80
            (iflag .&. 0x08) `shouldBe` 0x00

        it "writes to SC without bit 7 set do not capture" $ do
            b <- emptyBus
            write8 0xFF01 0x42 b
            write8 0xFF02 0x01 b
            ser <- drainSerial b
            ser `shouldBe` []

        it "draining serial clears buffered output" $ do
            b <- emptyBus
            mapM_
                (\ch -> write8 0xFF01 ch b >> write8 0xFF02 0x81 b)
                [0x4F, 0x4B]
            first <- drainSerial b
            second <- drainSerial b
            first `shouldBe` [0x4F, 0x4B]
            second `shouldBe` []

    describe "advance" $ do
        it "ticks the divider on each call" $ do
            b <- emptyBus
            advance 64 b
            v <- read8 0xFF04 b
            v `shouldBe` 0x01

        it "raises the Timer interrupt in IF on TIMA overflow (after the reload window)" $ do
            b <- emptyBus
            write8 0xFF06 0x10 b -- TMA = 0x10
            write8 0xFF05 0xFF b -- TIMA = 0xFF
            write8 0xFF07 0x05 b -- TAC = 0x05 (enable + 16 T-cycles)
            -- 4 M-cycles hits the falling edge that wraps TIMA, but the 1-M-cycle reload window
            -- has not yet expired; IF stays clear.
            advance 4 b
            iflag1 <- read8 0xFF0F b
            (iflag1 .&. 0x04) `shouldBe` 0x00
            -- One more M-cycle drains the reload window: TIMA := TMA, IF set.
            advance 1 b
            iflag2 <- read8 0xFF0F b
            (iflag2 .&. 0x04) `shouldBe` 0x04

    describe "OAM DMA via 0xFF46" $ do
        it "copies 160 bytes from (v << 8) into OAM at 0xFE00 starting next instruction" $ do
            b <- emptyBus
            mapM_ (\i -> write8 (0xC000 + fromIntegral i) (fromIntegral i) b) [0 .. 0x9F :: Int]
            -- Model an LD (0xFF46), A: write to FF46, then advance for the rest of that instruction's
            -- M-cycles. DMA must NOT have copied any bytes during the triggering instruction.
            write8 0xFF46 0xC0 b
            advance 3 b
            partway <- read8 0xFE00 b
            partway `shouldBe` 0xFF -- Locked, but more importantly: not yet copied
            -- 160 M-cycles of subsequent instruction time complete the copy, plus 1 deferred-clear
            -- cycle so 'busOamDmaActive' transitions from True to False (matches mooneye
            -- 'oam_dma_timing': a CPU read scheduled at the same M-cycle as the final byte still
            -- sees the lockout).
            advance 161 b
            v0 <- Ppu.read8 0xFE00 (Bus.busPpu b)
            v1 <- Ppu.read8 0xFE01 (Bus.busPpu b)
            v9F <- Ppu.read8 0xFE9F (Bus.busPpu b)
            v0 `shouldBe` 0x00
            v1 `shouldBe` 0x01
            v9F `shouldBe` 0x9F

        it "DMG: source 0xFExx mirrors WRAM via 'src & ~0x2000'" $ do
            -- 'emptyBus' synthesises a no-MBC ROM with the DMG-only header, so 'fromCartridge'
            -- picks 'HostDmg'. On DMG, an OAM DMA from source 0xFE00..0xFFFF reads through the echo
            -- mirror at 'src & ~0x2000' (here 0xDE00..0xDFFF). We seed the upper-WRAM byte that
            -- 0xFE00 should mirror to (0xDE00 -> WRAM offset 0x1E00) and verify byte 0 of OAM lands on it.
            b <- emptyBus
            write8 0xDE00 0x55 b -- = WRAM[0x1E00] via the upper bank
            write8 0xFF46 0xFE b -- DMA source = 0xFE00
            advance 1 b -- Consume the 1-cycle startup delay
            advance 161 b -- Finish the copy + 1 deferred-clear cycle
            v0 <- Ppu.read8 0xFE00 (Bus.busPpu b)
            v0 `shouldBe` 0x55

        it "DMG: source 0xFFxx mirrors WRAM via 'src & ~0x2000'" $ do
            -- Source 0xFF00..0xFFFF reads from 0xDF00..0xDFFF on DMG.
            -- Without the fix, this region returned 0xFF for every byte regardless of WRAM contents.
            b <- emptyBus
            write8 0xDF42 0xCD b -- = Upper WRAM byte that source 0xFF42 mirrors to
            write8 0xFF46 0xFF b -- DMA source = 0xFF00
            advance 1 b
            advance 161 b -- 160 copies + 1 deferred-clear cycle
            v42 <- Ppu.read8 0xFE42 (Bus.busPpu b)
            v42 `shouldBe` 0xCD

        {- OAM DMA occupies one internal bus, not the whole address space. A DMA sourced from VRAM
        leaves the main bus (ROM, SRAM, WRAM, echo) readable, and vice versa. Ocelot used to lock the
        CPU out of everything below 0xFF00 for the duration, which made an instruction fetch from
        WRAM or echo RAM return 0xFF; the CPU then decoded that as RST 38h and wedged at PC=0x38.
        That is the single cause behind all nine mooneye instruction-timing ROMs timing out.
        Mirrors SameBoy's @is_addr_in_dma_use@.
        -}
        -- 'advance 1' consumes the startup delay; copying only begins on the next call, so every
        -- case below advances twice when it wants the transfer genuinely under way. A single
        -- 'advance n' straight after the FF46 write copies nothing and leaves the index at 0.
        it "a VRAM-sourced DMA leaves the main bus readable" $ do
            b <- emptyBus
            write8 0xFF40 0x00 b -- LCD off: VRAM freely writable and the PPU frozen
            write8 0x8000 0x11 b -- byte the DMA will be reading
            write8 0xC000 0xAA b -- byte the CPU wants while the DMA runs
            write8 0xFF46 0x80 b -- DMA source = 0x8000, the VRAM bus
            advance 1 b
            advance 4 b -- 4 bytes copied: genuinely partway through
            wram <- read8 0xC000 b
            wram `shouldBe` 0xAA

        it "a VRAM-sourced DMA leaves echo RAM readable" $ do
            -- The echo window is where the crash actually bit: mooneye runs test code at 0xFDFE.
            b <- emptyBus
            write8 0xFF40 0x00 b
            write8 0xDDFE 0xCD b -- echo 0xFDFE mirrors 0xDDFE
            write8 0xFF46 0x80 b
            advance 1 b
            advance 4 b
            echo <- read8 0xFDFE b
            echo `shouldBe` 0xCD

        it "lets the CPU read back the address the DMA is currently sourcing" $ do
            -- SameBoy exempts this explicitly ("Shortcut for DMA access flow"): the byte is on the
            -- bus, so the CPU sees it rather than 0xFF.
            b <- emptyBus
            write8 0xC000 0x3C b
            write8 0xFF46 0xC0 b
            advance 1 b -- startup delay consumed; the DMA is about to source 0xC000
            v <- read8 0xC000 b
            v `shouldBe` 0x3C

        it "moves the exempt address along with the transfer" $ do
            -- Pins the index term in @cur = src + idx@. A page-aligned 160-byte transfer can never
            -- cross a bus boundary, so the index cannot change the bus classification; what it does
            -- change is which address the source exemption applies to. Drop the term and the
            -- exemption would stay stuck on 0xC000 for the whole transfer.
            b <- emptyBus
            write8 0xC000 0x11 b
            write8 0xC005 0x22 b
            write8 0xFF46 0xC0 b
            advance 1 b -- startup delay consumed
            advance 5 b -- 5 bytes copied, so the DMA is now sourcing 0xC005
            atCursor <- read8 0xC005 b
            behindCursor <- read8 0xC000 b
            atCursor `shouldBe` 0x22 -- exempt: the byte is on the bus
            behindCursor `shouldBe` 0xFF -- same bus, not the current source
        it "a RAM-bus-sourced DMA leaves VRAM readable on CGB" $ do
            b <- cgbBus
            write8 0xFF40 0x00 b
            write8 0x8000 0x77 b
            write8 0xFF46 0xC0 b -- DMA source = 0xC000, which is its own bus on CGB
            advance 1 b
            advance 4 b
            vram <- read8 0x8000 b
            vram `shouldBe` 0x77 -- RAM bus and VRAM bus are distinct on CGB
        it "a ROM-sourced DMA still blocks WRAM on CGB" $ do
            -- The converse, and the only cover for the @cgb && addr >= 0xC000@ guard: CGB giving WRAM
            -- its own bus does not make it readable while the DMA is on the main bus.
            b <- cgbBus
            write8 0xFF40 0x00 b
            write8 0xC800 0x99 b
            write8 0xFF46 0x00 b -- DMA source = 0x0000, the main bus
            advance 1 b
            advance 4 b
            wram <- read8 0xC800 b
            wram `shouldBe` 0xFF

        it "blocks main-bus reads but lets I/O regs and HRAM through" $ do
            b <- emptyBus
            mapM_ (\i -> write8 (0xC000 + fromIntegral i) 0xAA b) [0 .. 0x9F :: Int]
            write8 0xD000 0xAA b
            write8 0xFF80 0x55 b -- HRAM stays accessible
            write8 0xFF46 0xC0 b
            advance 4 b -- Partway through
            -- Probe 0xD000 rather than 0xC000: on DMG both are the main bus, but 0xC000 is the
            -- address this DMA is currently sourcing, and hardware lets the CPU read that one back
            -- off the bus (see the source-address exemption test below). Probing it conflated
            -- "the main bus is busy" with "the DMA's own source is unreadable".
            wramR <- read8 0xD000 b
            hramR <- read8 0xFF80 b
            -- FF46 lives in the I/O register page, so it stays readable during DMA and reflects the
            -- last-written source byte.
            regR <- read8 0xFF46 b
            wramR `shouldBe` 0xFF
            hramR `shouldBe` 0x55
            regR `shouldBe` 0xC0

    describe "joypad" $ do
        it "0xFF00 reads return 0xCF when no buttons are pressed" $ do
            b <- emptyBus
            v <- read8 0xFF00 b
            v `shouldBe` 0xCF

        it "0xFF00 row-select bits round-trip" $ do
            b <- emptyBus
            write8 0xFF00 0x10 b
            v <- read8 0xFF00 b
            v `shouldBe` 0xDF

        it "setButton routes frontend input through the bus" $ do
            b <- emptyBus
            write8 0xFF00 0x10 b -- Select action row.
            Bus.setButton ButtonA True b
            pressed <- read8 0xFF00 b
            Bus.setButton ButtonA False b
            released <- read8 0xFF00 b
            (pressed .&. 0x01) `shouldBe` 0x00
            (released .&. 0x01) `shouldBe` 0x01

    describe "frontend facade" $ do
        it "reports platform and double-speed state without exposing raw fields" $ do
            dmg <- emptyBus
            Bus.isCgb dmg `shouldBe` False
            Bus.isDoubleSpeed dmg `shouldReturn` False

            cgb <- cgbBus
            Bus.isCgb cgb `shouldBe` True
            Bus.isDoubleSpeed cgb `shouldReturn` False
            write8 0xFF4D 0x01 cgb
            Bus.triggerSpeedSwitch cgb `shouldReturn` True
            Bus.isDoubleSpeed cgb `shouldReturn` True

        it "exposes framebuffer snapshots through bus-level accessors" $ do
            b <- emptyBus
            let ppu = Bus.busPpu b

            palette <- Bus.framebuffer b
            paletteDirect <- Ppu.framebuffer ppu
            rgb <- Bus.framebufferRgb b
            rgbDirect <- Ppu.framebufferRgb ppu
            rgbBytes <- Bus.framebufferRgbBytes b
            rgbaBytes <- Bus.framebufferRgbaBytes b

            palette `shouldBe` paletteDirect
            rgb `shouldBe` rgbDirect
            rgbBytes `shouldBe` BS.pack (V.toList rgb)
            BS.length rgbaBytes `shouldBe` Ppu.framebufferWidth * Ppu.framebufferHeight * 4

        it "latches a completed visible frame on VBlank entry" $ do
            b <- emptyBus
            Bus.takeFrameReady b `shouldReturn` False
            advance 17556 b
            Bus.takeFrameReady b `shouldReturn` True
            Bus.takeFrameReady b `shouldReturn` False

    describe "boot ROM" $ do
        it "served from boot ROM bytes until 0xFF50 unmasks the cartridge" $ do
            b <- emptyBus
            -- A 256-byte boot ROM with byte i = i.
            installBootRom (BS.pack [fromIntegral (i :: Int) | i <- [0 .. 255]]) b
            v0 <- read8 0x0000 b
            vFF <- read8 0x00FF b
            v0 `shouldBe` 0x00
            vFF `shouldBe` 0xFF
            -- Unmask: write any non-zero value to 0xFF50.
            write8 0xFF50 0x01 b
            -- Now the read goes to the cartridge.
            v0' <- read8 0x0000 b
            v0' `shouldBe` 0x00 -- Synthetic cart is mostly zero
        it "0xFF50 is sticky: a second cleared boot mask cannot re-mask" $ do
            b <- emptyBus
            installBootRom (BS.pack (replicate 256 0xAA)) b
            write8 0xFF50 0x01 b
            -- Subsequent reads stay at the cartridge regardless of any attempt to re-enable.
            v <- read8 0x0000 b
            v `shouldBe` 0x00
