-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

{-# LANGUAGE RecordWildCards #-}

module MyParser (parseDKA) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Set (Set, fromList, toList)
import Text.Parsec (char, endBy, eof, many1, newline, parse,
    satisfy, sepBy1, alphaNum)
import Text.Parsec.String (Parser)

import MyData

--thsese functions parse the input file and store its data into DKA data type.
--parseDKA is the main fuction
parseDKA :: String -> Err DKA
parseDKA = validate <=< left show . parse parser "" 

parser :: Parser DKA
parser = DKA
  <$> stateSymbolParser <* newline
  <*> inputSymbolParser <* newline
  <*> startingSymbolParser <* newline
  <*> endingSymbolParser <* newline
  <*> rulesParser <* eof

stateSymbolParser :: Parser [DKAState]
stateSymbolParser = sepBy1 stateParser comma

stateParser :: Parser DKAState
stateParser = many1 alphaNum

inputSymbolParser :: Parser [InputSymbol]
inputSymbolParser = many1 inputSymbols

inputSymbols :: Parser InputSymbol
inputSymbols = satisfy (`elem` "abcdefghijklmnopqrstuvwxyz")

startingSymbolParser :: Parser DKAState
startingSymbolParser = many1 alphaNum

endingSymbolParser :: Parser [DKAState]
endingSymbolParser = sepBy1 stateParser comma

rulesParser :: Parser (Set Rule)
rulesParser = fromList <$> endBy ruleParser newline

ruleParser :: Parser Rule 
ruleParser = Rule
  <$> stateParser <* comma
  <*> inputSymbols <* comma
  <*> stateParser 

comma :: Parser Char
comma = char ','

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

-- validates if DKA is in correct format
validate :: DKA -> Err DKA
validate dka@DKA{..} =
  if isValid then Right dka else Left "ERROR: Input file is in invalid format"
  where
    isValid = startState `elem` states
           && subList endStates states
           && all ((`elem` states) . fromState) (toList rules)
           && all ((`elem` alphabet) . symbol) (toList rules)
           && all ((`elem` states) . toState) (toList rules)
           && allDifferent states
           && allDifferent endStates
           && allDifferent alphabet
