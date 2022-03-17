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
import MyConversion(convertToMKA)
import Data.Either

--load the arguments
main :: IO ()
main = do
    (action, inputFile) <- processArguments =<< getArgs
    either die action (parseDKA inputFile)

--execute the action based on the arguments
processArguments :: [String] -> IO (DKA -> IO (), String)
processArguments [x,y] = do
    inputFile <- readFile y
    case x of
     "-i" -> return (printDKA, inputFile)
     "-t" -> return (convertDKA, inputFile)
     _    -> die "Error: expecting two arguments: [-i|-t] FILENAME"
processArguments _ = die "Error: expecting two arguments: [-i|-t] FILENAME"

--print DKA loaded from file
printDKA :: DKA -> IO ()
printDKA = putStr . show

--run minimalisation algorithm on the DKA and print the result
convertDKA :: DKA -> IO ()
convertDKA = printDKA . convertToMKA
