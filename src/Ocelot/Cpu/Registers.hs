{-# LANGUAGE BangPatterns #-}

{- | The SM83 CPU register file.

The Gameboy CPU exposes eight 8-bit registers (A, F, B, C, D, E, H, L)
that can be addressed as four 16-bit pairs (AF, BC, DE, HL), plus a
16-bit stack pointer and program counter. The low nibble of F is hard-wired
to zero on real hardware, so writes through 'setAF' mask it off.
-}
module Ocelot.Cpu.Registers (
    Registers (..),
    Flag (..),
    zeroRegisters,
    dmgPostBoot,
    dmg0PostBoot,
    mgbPostBoot,
    sgbPostBoot,
    sgb2PostBoot,
    cgbPostBoot,
    cgbAPostBoot,
    cgbDmgCompatPostBoot,
    getAF,
    setAF,
    getBC,
    setBC,
    getDE,
    setDE,
    getHL,
    setHL,
    getFlag,
    setFlag,
) where

import Data.Bits (clearBit, setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Word (Word16, Word8)

data Registers = Registers
    { regA :: !Word8
    , regF :: !Word8
    , regB :: !Word8
    , regC :: !Word8
    , regD :: !Word8
    , regE :: !Word8
    , regH :: !Word8
    , regL :: !Word8
    , regSP :: !Word16
    , regPC :: !Word16
    }
    deriving (Eq, Show)

data Flag = FlagZ | FlagN | FlagH | FlagC
    deriving (Eq, Show, Bounded, Enum)

zeroRegisters :: Registers
zeroRegisters = Registers 0 0 0 0 0 0 0 0 0 0

{- | Register state immediately after the DMG internal boot ROM hands off to
the cartridge at @0x0100@. Used as the canonical starting state when no boot
ROM is supplied.
-}
dmgPostBoot :: Registers
dmgPostBoot =
    Registers
        { regA = 0x01
        , regF = 0xB0
        , regB = 0x00
        , regC = 0x13
        , regD = 0x00
        , regE = 0xD8
        , regH = 0x01
        , regL = 0x4D
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state immediately after the CGB boot ROM hands off to the
cartridge at @0x0100@. The key value is @A = 0x11@, which CGB-aware ROMs
check to decide whether to use the CGB palette pipeline; without this,
games leave BG\/OBJ palette RAM uninitialized and render all-white.
-}
cgbPostBoot :: Registers
cgbPostBoot =
    Registers
        { regA = 0x11
        , regF = 0x80
        , regB = 0x00
        , regC = 0x00
        , regD = 0xFF
        , regE = 0x56
        , regH = 0x00
        , regL = 0x0D
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state immediately after the DMG0 boot ROM (the very early
DMG hardware revision, before the "ABC" silicon stepping) hands off
to the cartridge. Distinct from 'dmgPostBoot' in B, E, H, L, and F:
the DMG0 boot ROM leaves the carry flag clear (F=0x00) and seeds
@HL=0x8403@ instead of @HL=0x014D@. Drives mooneye
@acceptance/boot_regs-dmg0@.
-}
dmg0PostBoot :: Registers
dmg0PostBoot =
    Registers
        { regA = 0x01
        , regF = 0x00
        , regB = 0xFF
        , regC = 0x13
        , regD = 0x00
        , regE = 0xC1
        , regH = 0x84
        , regL = 0x03
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state immediately after the MGB (Gameboy Pocket) boot
ROM hands off. Identical to 'dmgPostBoot' except @A = 0xFF@ instead
of @A = 0x01@. Drives mooneye @acceptance/boot_regs-mgb@.
-}
mgbPostBoot :: Registers
mgbPostBoot =
    Registers
        { regA = 0xFF
        , regF = 0xB0
        , regB = 0x00
        , regC = 0x13
        , regD = 0x00
        , regE = 0xD8
        , regH = 0x01
        , regL = 0x4D
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state immediately after the SGB (Super Gameboy) boot
ROM hands off. Drives mooneye @acceptance/boot_regs-sgb@.
-}
sgbPostBoot :: Registers
sgbPostBoot =
    Registers
        { regA = 0x01
        , regF = 0x00
        , regB = 0x00
        , regC = 0x14
        , regD = 0x00
        , regE = 0x00
        , regH = 0xC0
        , regL = 0x60
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state immediately after the SGB2 boot ROM hands off.
Identical to 'sgbPostBoot' except @A = 0xFF@. Drives mooneye
@acceptance/boot_regs-sgb2@.
-}
sgb2PostBoot :: Registers
sgb2PostBoot =
    Registers
        { regA = 0xFF
        , regF = 0x00
        , regB = 0x00
        , regC = 0x14
        , regD = 0x00
        , regE = 0x00
        , regH = 0xC0
        , regL = 0x60
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state on a CGB chip revision A running a DMG-only
cartridge ("DMG-compat mode"). Distinct from 'cgbPostBoot' (which
covers the CGB-aware-cart handoff): when a DMG-only cart boots on
CGB hardware the boot ROM seeds different register values to keep
the cart compatible. Drives mooneye @misc/boot_regs-A@.
-}
cgbAPostBoot :: Registers
cgbAPostBoot =
    Registers
        { regA = 0x11
        , regF = 0x00
        , regB = 0x01
        , regC = 0x00
        , regD = 0x00
        , regE = 0x08
        , regH = 0x00
        , regL = 0x7C
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

{- | Register state on a regular CGB (revisions B-E) running a DMG-only
cartridge ("DMG-compat mode"). Distinct from 'cgbPostBoot' (which
covers the CGB-aware-cart handoff). Drives mooneye
@misc/boot_regs-cgb@.
-}
cgbDmgCompatPostBoot :: Registers
cgbDmgCompatPostBoot =
    Registers
        { regA = 0x11
        , regF = 0x80
        , regB = 0x00
        , regC = 0x00
        , regD = 0x00
        , regE = 0x08
        , regH = 0x00
        , regL = 0x7C
        , regSP = 0xFFFE
        , regPC = 0x0100
        }

pack16 :: Word8 -> Word8 -> Word16
pack16 hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

hi8 :: Word16 -> Word8
hi8 w = fromIntegral (w `shiftR` 8)

lo8 :: Word16 -> Word8
lo8 w = fromIntegral (w .&. 0xFF)

getAF :: Registers -> Word16
getAF r = pack16 (regA r) (regF r)

setAF :: Word16 -> Registers -> Registers
setAF !w r = r{regA = hi8 w, regF = lo8 w .&. 0xF0}

getBC :: Registers -> Word16
getBC r = pack16 (regB r) (regC r)

setBC :: Word16 -> Registers -> Registers
setBC !w r = r{regB = hi8 w, regC = lo8 w}

getDE :: Registers -> Word16
getDE r = pack16 (regD r) (regE r)

setDE :: Word16 -> Registers -> Registers
setDE !w r = r{regD = hi8 w, regE = lo8 w}

getHL :: Registers -> Word16
getHL r = pack16 (regH r) (regL r)

setHL :: Word16 -> Registers -> Registers
setHL !w r = r{regH = hi8 w, regL = lo8 w}

flagBit :: Flag -> Int
flagBit FlagZ = 7
flagBit FlagN = 6
flagBit FlagH = 5
flagBit FlagC = 4

getFlag :: Flag -> Registers -> Bool
getFlag f r = testBit (regF r) (flagBit f)

setFlag :: Flag -> Bool -> Registers -> Registers
setFlag f True r = r{regF = setBit (regF r) (flagBit f)}
setFlag f False r = r{regF = clearBit (regF r) (flagBit f)}
