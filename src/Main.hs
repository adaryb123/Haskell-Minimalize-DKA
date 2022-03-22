-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

module Main where

import System.IO  
import System.Environment (getArgs)
import System.Exit (exitFailure)
import MyData
import MyParser(parseDKA)
import MyConversion(convertToMKA)

--load the arguments
main :: IO ()
main = do
    (action, inputFile) <- processArgs =<< getArgs
    either terminate action (parseDKA inputFile)

--read the input from file or stdin
processArgs :: [String] -> IO (DKA -> IO (), String)
processArgs [option] = processOptions option =<< getContents
processArgs [option, inputFile] = processOptions option =<< readFile inputFile
processArgs _ = terminate "Excepting an option argument and optionally an input file: flp21-fun (-i|-t) [input-file]"

--print the input DKA or the converted MKA
processOptions :: String -> String -> IO (DKA -> IO (), String)
processOptions option input = case option of
  "-i" -> return (printDKA, input)
  "-t" -> return (convertDKA, input)
  _ -> terminate $ "Unknown option: " ++ option

--print DKA loaded from file
printDKA :: DKA -> IO ()
printDKA = putStr . show

--run minimalisation algorithm on the DKA and print the result
convertDKA :: DKA -> IO ()
convertDKA = printDKA . convertToMKA

--terminates the program with an error code and an error message
terminate :: String -> IO a
terminate str = hPutStrLn stderr str >> exitFailure