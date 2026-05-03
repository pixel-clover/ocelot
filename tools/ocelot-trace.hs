{-# LANGUAGE OverloadedStrings #-}

{- | Per-instruction CPU trace, format-matched with @sameboy-trace@. Pipe both side by side through
'diff -u' (or @tools/diff-traces@) to find the first instruction at which Ocelot and SameBoy diverge
for a given ROM.

Output line format (matches sameboy-trace.c):

> pc=XXXX af=XXXX bc=XXXX de=XXXX hl=XXXX sp=XXXX if=XX ie=XX ly=XXX lcdc=XX

One line per CPU instruction. Trace starts at the cart entry point (PC=0x100, post-boot CGB register state)
and emits the requested number of lines.
-}
module Main (main) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import Data.Word (Word16, Word8)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (step)
import Ocelot.Cpu.Registers
    ( regA
    , regB
    , regC
    , regD
    , regE
    , regF
    , regH
    , regL
    , regPC
    , regSP
    )
import Ocelot.Cpu.State (CpuState (..))
import Ocelot.Machine (Machine (..), machineFromCartridgeWithBoot)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    (path, n) <- case args of
        [p, c] -> pure (p, read c :: Int)
        _ -> putStrLn "usage: ocelot-trace <rom> <instruction-count>" >> exitFailure
    bytes <- BS.readFile path
    Right cart <- Cartridge.loadRom bytes
    -- Run with the same minimal boot stub as @tools/sameboy-trace.c@ so both emulators reach the
    -- cart entry point at PC=0x100 with identical T-cycle accumulation in every peripheral.
    -- Without this the SameBoy boot stub burns ~900 T-cycles of PPU phase that Ocelot's 'cgbPostBoot'
    -- shortcut does not, and traces diverge in LY by line ~30 even when the CPU/MBC are bit-identical.
    m <- machineFromCartridgeWithBoot (Just bootStub) cart
    hSetBuffering stdout LineBuffering

    -- SameBoy's execution callback fires only on real instruction starts, not while the CPU is
    -- halted in the wait-for-IRQ loop or while it's stalled on an interrupt-service entry.
    -- Mirror that by only emitting a line when the CPU is in instruction-fetch state (not halted).
    -- This makes line numbers in the two traces align 1:1 even though Ocelot's `step` returns one
    -- halt-tick at a time. Skip the boot stub instructions entirely; only start emitting once PC
    -- reaches the cart entry at 0x100 (mirrors sameboy-trace's 'reached_cart' gate).
    let drive !reached !left
            | left <= 0 = pure ()
            | otherwise = do
                cpu <- readIORef (machineCpu m)
                let pc = regPC (cpuRegs cpu)
                let nowReached = reached || pc == 0x0100
                if cpuHalted cpu
                    then step m >> drive nowReached left
                    else
                        if nowReached
                            then emit m >> step m >> drive nowReached (left - 1)
                            else step m >> drive nowReached left
    drive False n

emit :: Machine -> IO ()
emit m = do
    cpu <- readIORef (machineCpu m)
    let r = cpuRegs cpu
        af = w16 (regA r) (regF r)
        bc = w16 (regB r) (regC r)
        de = w16 (regD r) (regE r)
        hl = w16 (regH r) (regL r)
    iflag <- Bus.read8 0xFF0F (machineBus m)
    ie <- Bus.read8 0xFFFF (machineBus m)
    ly <- Bus.read8 0xFF44 (machineBus m)
    lcdc <- Bus.read8 0xFF40 (machineBus m)
    printf
        "pc=%04X af=%04X bc=%04X de=%04X hl=%04X sp=%04X if=%02X ie=%02X ly=%03d lcdc=%02X\n"
        (regPC r)
        af
        bc
        de
        hl
        (regSP r)
        iflag
        ie
        ly
        lcdc

w16 :: Word8 -> Word8 -> Word16
w16 hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

{- | Boot stub byte-identical to @boot_stub@ in @tools/sameboy-trace.c@.
Both tools must execute the same 256 bytes so peripheral T-cycle accumulation matches at PC=0x100.
If you edit one side, edit the other.
-}
bootStub :: BS.ByteString
bootStub =
    BS.pack $
        replicate 0x100 0x00
            -- XOR A: sets F=0x80 (Z=1, N=H=C=0), Pan Docs CGB post-boot.
            & set 0x00 0xAF
            -- LD A, 0x91 ; LDH (FF40), A   -- LCDC = 0x91
            & set 0xD9 0x3E
            & set 0xDA 0x91
            & set 0xDB 0xE0
            & set 0xDC 0x40
            -- LD A, 0xFC ; LDH (FF47), A   -- BGP = 0xFC
            & set 0xDD 0x3E
            & set 0xDE 0xFC
            & set 0xDF 0xE0
            & set 0xE0 0x47
            -- LD A, 0xFF ; LDH (FF48), A ; LDH (FF49), A   -- OBP0/OBP1 = 0xFF
            & set 0xE1 0x3E
            & set 0xE2 0xFF
            & set 0xE3 0xE0
            & set 0xE4 0x48
            & set 0xE5 0xE0
            & set 0xE6 0x49
            -- LD A, 0x77 ; LDH (FF24), A   -- NR50
            & set 0xE7 0x3E
            & set 0xE8 0x77
            & set 0xE9 0xE0
            & set 0xEA 0x24
            -- LD A, 0xF3 ; LDH (FF25), A   -- NR51
            & set 0xEB 0x3E
            & set 0xEC 0xF3
            & set 0xED 0xE0
            & set 0xEE 0x25
            -- LD A, 0x80 ; LDH (FF26), A   -- NR52 (APU on)
            & set 0xEF 0x3E
            & set 0xF0 0x80
            & set 0xF1 0xE0
            & set 0xF2 0x26
            -- LD D, 0xFF ; LD E, 0x56 ; LD L, 0x0D ; LD SP, 0xFFFE
            & set 0xF3 0x16
            & set 0xF4 0xFF
            & set 0xF5 0x1E
            & set 0xF6 0x56
            & set 0xF7 0x2E
            & set 0xF8 0x0D
            & set 0xF9 0x31
            & set 0xFA 0xFE
            & set 0xFB 0xFF
            -- LD A, 0x11 ; LDH (FF50), A   -- Unmap & hand off to cart at 0x100
            & set 0xFC 0x3E
            & set 0xFD 0x11
            & set 0xFE 0xE0
            & set 0xFF 0x50
  where
    set i v xs = take i xs <> [v] <> drop (i + 1) xs
    (&) = flip ($)
    infixl 1 &
