module Main where

import System.IO  
import Parser(parseDKA)

main = do 
    contents <- readFile "../test/test1input.txt"  
    let contentString = contents
    print $ parseDKA contentString
