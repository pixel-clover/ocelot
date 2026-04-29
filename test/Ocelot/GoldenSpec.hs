{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | ROM-driven golden tests.

These tests run real test ROMs end-to-end and check the result against a known-good output.
They live behind file-existence guards so the test suite still passes on a clean checkout that
hasn't fetched the @external/gb-test-roms@ submodule.

* blargg ROMs print their status to the serial port; we run until we see @\"Passed\"@ or
  @\"Failed\"@ or hit an instruction cap.
* dmg-acid2 writes a fixed image to the framebuffer; we hash the framebuffer bytes after a
  fixed run and compare against a stored hash.
-}
module Ocelot.GoldenSpec (spec) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isInfixOf)
import qualified Ocelot.Bus as Bus
import qualified Ocelot.Cartridge as Cartridge
import Ocelot.Cpu.Execute (runFor)
import Ocelot.Machine (Machine (..), machineFromCartridge)
import System.Environment (lookupEnv)
import Test.Hspec

{- | Maximum instructions to run a blargg ROM before giving up on
finding a Passed/Failed verdict. Tuned so each test finishes in a
few seconds on a modern host.
-}
blarggCap :: Int
blarggCap = 80_000_000

{- | Run a blargg ROM, polling the serial port every 'pollChunk'
instructions for a verdict.
-}
pollChunk :: Int
pollChunk = 1_000_000

{- | The golden suite is opt-in to keep the inner-loop test run fast.
Set @OCELOT_GOLDEN=1@ to actually run the ROMs; otherwise each test
pends with a hint.
-}
goldenEnabled :: IO Bool
goldenEnabled = do
    v <- lookupEnv "OCELOT_GOLDEN"
    pure (v == Just "1")

skipUnlessGolden :: IO () -> IO ()
skipUnlessGolden act = do
    on <- goldenEnabled
    if on then act else pendingWith "set OCELOT_GOLDEN=1 to run golden ROM tests"

spec :: Spec
spec = do
    describe "blargg cpu_instrs (individual)" $
        mapM_
            blarggCase
            [ "01-special"
            , "02-interrupts"
            , "03-op sp,hl"
            , "04-op r,imm"
            , "05-op rp"
            , "06-ld r,r"
            , "07-jr,jp,call,ret,rst"
            , "08-misc instrs"
            , "09-op r,r"
            , "10-bit ops"
            , "11-op a,(hl)"
            ]

    describe "blargg instr_timing" $
        it "reports Passed via serial" $
            skipUnlessGolden $ do
                mb <- tryReadFile "external/gb-test-roms/instr_timing/instr_timing.gb"
                case mb of
                    Nothing -> pendingWith "external/gb-test-roms submodule not initialized"
                    Just bytes -> runBlarggAssertPass bytes

    describe "dmg-acid2" $
        it "framebuffer hash matches the reference" $
            skipUnlessGolden $ do
                mb <- tryReadFile "external/dmg-acid2/dmg-acid2.gb"
                case mb of
                    Nothing -> pendingWith "external/dmg-acid2/dmg-acid2.gb not present"
                    Just _bytes ->
                        -- We have no captured reference hash yet; once the
                        -- ROM is added a follow-up should run it once,
                        -- record the hash, and replace this expectation.
                        pendingWith "reference hash not yet captured"

blarggCase :: String -> Spec
blarggCase name =
    it (name <> " reports Passed via serial") $ skipUnlessGolden $ do
        let path = "external/gb-test-roms/cpu_instrs/individual/" <> name <> ".gb"
        mb <- tryReadFile path
        case mb of
            Nothing -> pendingWith "external/gb-test-roms submodule not initialized"
            Just bytes -> runBlarggAssertPass bytes

runBlarggAssertPass :: ByteString -> Expectation
runBlarggAssertPass bytes = do
    r <- Cartridge.loadRom bytes
    case r of
        Left e -> expectationFailure ("loadRom: " <> show e)
        Right cart -> do
            m <- machineFromCartridge cart
            verdict <- runUntilVerdict blarggCap m
            case verdict of
                BlarggPassed out ->
                    -- Sanity: the serial transcript should at minimum
                    -- be non-empty and ASCII-printable-ish.
                    BS.length out `shouldSatisfy` (> 0)
                BlarggFailed out ->
                    expectationFailure ("ROM reported Failed: " <> BSC.unpack out)
                BlarggTimeout out ->
                    expectationFailure
                        ( "ROM did not produce a verdict in "
                            <> show blarggCap
                            <> " instructions; serial so far: "
                            <> BSC.unpack out
                        )

data Verdict
    = BlarggPassed !ByteString
    | BlarggFailed !ByteString
    | BlarggTimeout !ByteString

runUntilVerdict :: Int -> Machine -> IO Verdict
runUntilVerdict cap m = go 0 BS.empty
  where
    go n acc
        | n >= cap = pure (BlarggTimeout acc)
        | otherwise = do
            _ <- runFor pollChunk m
            chunk <- Bus.drainSerial (machineBus m)
            let acc' = acc <> BS.pack chunk
                s = BSC.unpack acc'
            if "Passed" `isInfixOf` s
                then pure (BlarggPassed acc')
                else
                    if "Failed" `isInfixOf` s
                        then pure (BlarggFailed acc')
                        else go (n + pollChunk) acc'

tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile path = do
    r <- try (BS.readFile path) :: IO (Either IOException ByteString)
    pure (either (const Nothing) Just r)
