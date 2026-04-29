{-# LANGUAGE OverloadedStrings #-}

module Ocelot.IntegrationSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Ocelot.Bus as Bus
import Ocelot.Cpu.Execute (runUntilHalt)
import Ocelot.Machine (Machine (..))
import Ocelot.Testing (machineWithProgram)
import Test.Hspec

run :: [Word8] -> IO Machine
run bytes = do
    m <- machineWithProgram (BS.pack bytes)
    _ <- runUntilHalt 10000 m
    pure m

spec :: Spec
spec = do
    describe "end-to-end pipeline" $ do
        it "a small program writes a string through the serial port and halts" $ do
            let prog =
                    [ 0x21
                    , 0x10
                    , 0x00 -- LD HL, 0x0010
                    , 0x2A -- LD A, (HL+)
                    , 0xB7 -- OR A
                    , 0x28
                    , 0x08 -- JR Z, +8 -> HALT
                    , 0xE0
                    , 0x01 -- LDH (0x01), A   (SB)
                    , 0x3E
                    , 0x81 -- LD A, 0x81
                    , 0xE0
                    , 0x02 -- LDH (0x02), A   (SC := transfer)
                    , 0x18
                    , 0xF4 -- JR -12          -> LD A, (HL+)
                    , 0x76 -- HALT
                    , 0x4F
                    , 0x43
                    , 0x45
                    , 0x4C
                    , 0x4F
                    , 0x54
                    , 0x0A
                    , 0x00 -- "OCELOT\n\0"
                    ]
            m <- run prog
            ser <- Bus.drainSerial (machineBus m)
            ser `shouldBe` [0x4F, 0x43, 0x45, 0x4C, 0x4F, 0x54, 0x0A]
