module Main where

import System.IO  
import System.Environment (getArgs)
import System.Exit (die)
import MyData
import MyParser(parseDKA)
import MyConversion(convertToMKA)
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
     _    -> die "Error: expecting two arguments: [-i|-s] FILENAME"
procArgs _ = die "Error: expecting two arguments: [-i|-s] FILENAME"

printDKA :: DKA -> IO ()
printDKA = putStr . show

convertDKA :: DKA -> IO ()
convertDKA = printTable . convertDKAtoTable . convertToMKA

printTable :: AlgorithmTable -> IO ()
printTable = putStr . show