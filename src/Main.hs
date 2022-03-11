module Main where

import System.IO  
import System.Environment (getArgs)
import System.Exit (die)
import MyData
import MyParser(parseDKA)

-- main = do 
--     contents <- readFile "../test/test1input.txt"  
--     let contentString = contents
--     print $ parseDKA contentString

main :: IO ()
main = do
    (action, inputFile) <- procArgs =<< getArgs
    either die action (parseDKA inputFile)

-- Zpracování příkazového řádku
procArgs :: [String] -> IO (DKA -> IO (), String)
procArgs [x,y] = do
    inputFile <- readFile y
    case x of
     "-i" -> return (printDKA, inputFile)
     "-s" -> return (printDKA, inputFile)
     _    -> die "Error: expecting two arguments: [-i|-s] FILENAME"
procArgs _ = die "Error: expecting two arguments: [-i|-s] FILENAME"

printDKA :: DKA -> IO ()
printDKA = putStr . show