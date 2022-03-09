{-# LANGUAGE RecordWildCards, TupleSections #-}

module Data where

import Control.Arrow (first)
import Control.Monad (join)
import Data.List (intercalate, dropWhileEnd, unfoldr)
import Data.Set (Set, toList)

data DKA = DKA {states :: [DKAState],
                alphabet :: [InputSymbol],
                startState :: DKAState,
                endStates :: [DKAState],
                transitions :: Set Rule}

data Rule = Rule {fromState :: DKAState
                 , symbol :: InputSymbol
                 , toState :: DKAState} deriving (Eq,Ord)

type Err = Either String
type DKAState = String
type InputSymbol = Char

instance Show DKA where
    show DKA{..} = unwords $ ["\nStates: " ++ intercalate "," states ++ "\nAlphabet: " ++ alphabet ++ "\nStarting state: " ++ startState ++ "\nEnding states: " ++ intercalate "," endStates ++"\nTransitions:\n"] ++ map show (toList transitions)
 
instance Show Rule where
    show Rule{..} = fromState ++ "--" ++ show symbol ++ "-> " ++ toState ++ "\n"
        