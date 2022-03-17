-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO  
import System.Environment (getArgs)
import System.Exit (die)
import MyData
import MyParser(parseDKA)
import MyConversion(convertToMKA,convertTableToDKA,runAlgorithm,convertDKAtoTable)
import Data.Either


main :: IO ()
main = do
    (action, inputFile) <- procArgs =<< getArgs
    either die action (parseDKA inputFile)

procArgs :: [String] -> IO (DKA -> IO (), String)
procArgs [x,y] = do
    inputFile <- readFile y
    case x of
     "-i" -> return (printDKA, inputFile)
     "-t" -> return (convertDKA, inputFile)
     _    -> die "Error: expecting two arguments: [-i|-t] FILENAME"
procArgs _ = die "Error: expecting two arguments: [-i|-t] FILENAME"

printDKA :: DKA -> IO ()
printDKA = putStr . show

convertDKA :: DKA -> IO ()
convertDKA  dka' = printDKA ( convertTableToDKA (runAlgorithm ( convertDKAtoTable ( convertToMKA dka'))) dka')
