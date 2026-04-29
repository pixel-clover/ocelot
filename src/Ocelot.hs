{-# LANGUAGE OverloadedStrings #-}

{- | Public facade for the Ocelot Game Boy / Game Boy Color emulator.

This module re-exports the deliberate public surface of the library. As
subsystems land (CPU, MMU, PPU, APU), additional types and operations will be
exposed here. Internal coordination types (e.g. @PpuState@, @CpuState@) are
not re-exported.
-}
module Ocelot (
    version,

    -- * Cartridge
    Cartridge,
    CartridgeError (..),
    loadRom,
    cartridgeHeader,
    parseHeader,
    HeaderError (..),
    Header (..),
    CgbFlag (..),
    Destination (..),
    MbcKind (..),
    Capabilities (..),
) where

import Data.Text (Text)
import Ocelot.Cartridge (Cartridge, CartridgeError (..), cartridgeHeader, loadRom)
import Ocelot.Cartridge.Header (
    Capabilities (..),
    CgbFlag (..),
    Destination (..),
    Header (..),
    HeaderError (..),
    MbcKind (..),
    parseHeader,
 )

-- | Human-readable version string for the emulator library.
version :: Text
version = "Ocelot 0.1.0.0"
