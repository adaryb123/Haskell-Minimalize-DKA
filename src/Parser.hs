{-# LANGUAGE RecordWildCards #-}

module Parser where

import System.IO  
import Data.Typeable

import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
-- import Data.List (group, sort)
import Data.Set (Set, fromList, toList)
import Text.Parsec (char, count, endBy, eof, many1, newline, oneOf, parse,
    satisfy, sepBy, sepBy1, string, alphaNum)
import Text.Parsec.String (Parser)

import Data


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

-- parseAnything :: Parser [Char]
-- parseAnything = many1 anySymbol

-- anySymbol :: Parser Char
-- anySymbol = satisfy (`elem` "abcdefghijklmnopqrstuvwxyz 123456790,./';][] \n")


-- oneInputSymbolParser :: Parser InputSymbol
-- oneInputSymbolParser =  oneOf "abcdefghijklmnopqrstuvwxyz"

-- stateSymbols :: [Int]
-- stateSymbols = [0 ..]




validate :: DKA -> Err DKA
validate dka@DKA{..} =
  if isValid then Right dka else Left "ERROR"
  where
    -- allUnique l = all ((==) 1 . length) $ (group . sort) l
    isValid = True
      -- allUnique nonterminals
      -- && allUnique terminals
      -- && elem startingSymbol nonterminals
      -- && allUnique rules
      -- && all ( \(l, r) ->
      --   elem l nonterminals
      --   && all (`elem` nonterminals ++ terminals ++ [epsSymbol]) r
      -- ) rules
