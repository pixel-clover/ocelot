{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import Ocelot (
    Header (..),
    loadRom,
    parseHeader,
    version,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    TIO.putStrLn version
    args <- getArgs
    case args of
        [] -> putStrLn "usage: ocelot <rom-path>"
        (path : _) -> describeRom path

describeRom :: FilePath -> IO ()
describeRom path = do
    r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case r of
        Left e -> die ("could not read ROM file " <> path <> ": " <> show e)
        Right bytes -> case parseHeader bytes of
            Left err -> die ("invalid cartridge header: " <> show err)
            Right hdr -> do
                printHeader hdr
                case loadRom bytes of
                    Right _ -> putStrLn "status:   cartridge loaded"
                    Left err ->
                        putStrLn ("status:   not yet supported (" <> show err <> ")")

printHeader :: Header -> IO ()
printHeader h = do
    TIO.putStrLn ("title:    " <> hdrTitle h)
    putStrLn ("mbc:      " <> show (hdrMbcKind h))
    putStrLn ("rom:      " <> show (hdrRomBytes h `div` 1024) <> " KiB")
    putStrLn ("ram:      " <> show (hdrRamBytes h `div` 1024) <> " KiB")
    putStrLn ("cgb flag: " <> show (hdrCgbFlag h))
    putStrLn ("sgb flag: " <> show (hdrSgbFlag h))
    putStrLn ("dest:     " <> show (hdrDestination h))
    putStrLn ("version:  " <> show (hdrVersion h))

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
