{-# LANGUAGE RecordWildCards #-}

module MyParser (parseDKA) where

import System.IO  
import Data.Typeable

import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Set (Set, fromList, toList)
import Text.Parsec (char, count, endBy, eof, many1, newline, oneOf, parse,
    satisfy, sepBy, sepBy1, string, alphaNum)
import Text.Parsec.String (Parser)

import MyData


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

validate :: DKA -> Err DKA
validate dka@DKA{..} =
  if isValid then Right dka else Left "ERROR: Input file is in invalid format"
  where
    isValid = startState `elem` states
           && subList endStates states
           && all ((`elem` states) . fromState) rules
           && all ((`elem` alphabet) . symbol) rules
           && all ((`elem` states) . toState) rules
           && allDifferent states
           && allDifferent endStates
           && allDifferent alphabet
