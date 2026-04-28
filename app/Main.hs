{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Ocelot (version)
import System.Environment (getArgs)

main :: IO ()
main = do
    TIO.putStrLn version
    args <- getArgs
    case args of
        [] -> putStrLn "usage: ocelot <rom-path>"
        (rom : _) -> putStrLn $ "TODO: load ROM " ++ rom
