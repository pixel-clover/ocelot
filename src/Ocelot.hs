{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#ifndef wasm32_HOST_ARCH
{-# LANGUAGE TemplateHaskell #-}
#endif

{- HLINT ignore "Unused LANGUAGE pragma" -}

{- | Public facade for the Ocelot emulator.

This module re-exports the deliberate public surface of the library.
As subsystems land (CPU, MMU, PPU, and APU), additional types and operations will be exposed here.
Internal coordination types (e.g. @PpuState@, @CpuState@) are not re-exported.
-}
module Ocelot (
    version,

    -- * Cartridge
    Cartridge,
    CartridgeError (..),
    loadRom,
    cartridgeHeader,
    cartridgeHasBattery,
    extractRam,
    loadRam,
    extractSave,
    loadSave,
    parseHeader,
    HeaderError (..),
    Header (..),
    CgbFlag (..),
    Destination (..),
    MbcKind (..),
    Capabilities (..),
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Paths_ocelot as Paths
#ifndef wasm32_HOST_ARCH
import GitHash (giBranch, giHash, tGitInfoCwdTry)
#endif
import Ocelot.Cartridge (
    Cartridge,
    CartridgeError (..),
    cartridgeHasBattery,
    cartridgeHeader,
    extractRam,
    extractSave,
    loadRam,
    loadRom,
    loadSave,
 )
import Ocelot.Cartridge.Header (
    Capabilities (..),
    CgbFlag (..),
    Destination (..),
    Header (..),
    HeaderError (..),
    MbcKind (..),
    parseHeader,
 )

-- | Human-readable version string, including branch and short commit hash baked in at compile time.
version :: Text
#ifdef wasm32_HOST_ARCH
version = "Ocelot " <> T.pack (showVersion Paths.version)
#else
version = "Ocelot " <> T.pack (showVersion Paths.version) <> T.pack meta
  where
    meta = either (const "") (\gi -> " (" <> giBranch gi <> "@" <> take 5 (giHash gi) <> ")") $$tGitInfoCwdTry
#endif
