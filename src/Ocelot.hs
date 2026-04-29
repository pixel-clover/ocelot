{-# LANGUAGE OverloadedStrings #-}

{- | Public facade for the Ocelot Game Boy / Game Boy Color emulator.

This module is intentionally thin while the project is bootstrapping. As
subsystems land (CPU, MMU, PPU, APU, cartridge), the deliberate public
surface (e.g. a 'Machine' type and frame-stepping entry point) will be
exposed here. Internal coordination types should not be re-exported.
-}
module Ocelot (
    version,
) where

import Data.Text (Text)

-- | Human-readable version string for the emulator library.
version :: Text
version = "Ocelot 0.1.0.0"
