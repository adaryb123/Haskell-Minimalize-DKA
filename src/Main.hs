-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

module Main where
 
import System.Environment (getArgs)
import System.Exit (exitFailure)
import MyData
import MyParser(parseDKA)
import MyConversion(convertToMKA)

--load the arguments
main :: IO ()
main = do
    (action, inputFile) <- processArguments =<< getArgs
    either terminate action (parseDKA inputFile)

--read the input from file or stdin
processArguments :: [String] -> IO (DKA -> IO (), String)
processArguments [option] = executeAction option =<< getContents
processArguments [option, inputFile] = executeAction option =<< readFile inputFile
processArguments _ = terminate "Error: unknown arguments\n"

--print the input DKA or the converted MKA
executeAction :: String -> String -> IO (DKA -> IO (), String)
executeAction option input
  | option == "-i" = return (printDKA, input)
  | option == "-t" = return (convertDKA, input)
  | otherwise = terminate "Error: unknown arguments\n"

--print DKA loaded from file
printDKA :: DKA -> IO ()
printDKA = putStr . show

--run minimalisation algorithm on the DKA and print the result
convertDKA :: DKA -> IO ()
convertDKA = printDKA . convertToMKA

--end the program and print error message
terminate :: String -> IO a
terminate str = putStr str >> exitFailure