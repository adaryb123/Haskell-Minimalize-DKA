-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

{-# LANGUAGE RecordWildCards #-}

module MyData where

import Data.List (intercalate, dropWhileEnd, unfoldr, (\\))
import Data.Set (Set, toList, fromList)

-- deterministic finite-state automata
data DKA = DKA {states :: [DKAState],
                alphabet :: [InputSymbol],
                startState :: DKAState,
                endStates :: [DKAState],
                rules :: Set Rule}

-- rule in finite-state automata, showing the transitions between nodes
data Rule = Rule {fromState :: DKAState
                 , symbol :: InputSymbol
                 , toState :: DKAState} deriving (Ord)

--error output, in case of bad input
type Err = Either String
-- 1 node/state of the automata
type DKAState = String
-- 1 symbol of the automata alphabet
type InputSymbol = Char

--getter functions
getStates :: DKA -> [DKAState]
getStates (DKA x _ _ _ _) = x

getAlphabet :: DKA -> [InputSymbol]
getAlphabet (DKA _ x _ _ _) = x

getStartingState :: DKA -> DKAState
getStartingState (DKA _ _ x _ _) = x

getEndingStates :: DKA -> [DKAState]
getEndingStates (DKA _ _ _ x _) = x

getRules :: DKA -> Set Rule
getRules (DKA _ _ _ _ x) = x

getRuleFromState :: Rule -> DKAState
getRuleFromState (Rule x _ _) = x

getRuleSymbol :: Rule -> InputSymbol
getRuleSymbol (Rule _ x _) = x

getRuleToState :: Rule -> DKAState
getRuleToState (Rule _ _ x) = x

--printing functions
instance Show DKA where
    show DKA{..} = unwords $ (intercalate "," states ++ "\n" ++ alphabet ++ "\n" ++ startState ++ "\n" ++ intercalate "," endStates) : map show (toList rules)
    --show DKA{..} = unwords $ ["Printing DKA:\nStates: " ++ intercalate "," states ++ "\nAlphabet: " ++ alphabet ++ "\nStarting state: " ++ startState ++ "\nEnding states: " ++ intercalate "," endStates ++"\nRules:\n"] ++ map show (toList rules)

instance Show Rule where
    show Rule{..} = "\n" ++ fromState ++ "," ++ [symbol] ++ "," ++ toState
    -- show Rule{..} = fromState ++ "--" ++ show symbol ++ "-> " ++ toState ++ "\n"

-- comparing function
instance Eq Rule where
        (Rule x1 y1 z1) == (Rule x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2


-- table representing 1 step in the finite-state minimalisation algorithm.
-- This data type is equivalent to DKA
type AlgorithmTable = [TableRow]

-- 1 row of the table
data TableRow = TableRow{tableClass :: String
    , state :: DKAState
    , transitions :: [Transition]
    , previousClass :: String
}

-- transitions to the other table rows, this data type is somewhat equivalent to Rule 
data Transition = Transition{fromClass :: String
    , tToState :: DKAState
    , tSymbol :: InputSymbol
    , toClass :: String
} deriving (Ord)

--getter functions
getClass :: TableRow -> String
getClass (TableRow x _ _ _) = x

getState :: TableRow -> DKAState
getState (TableRow _ x _ _) = x

getTransitions :: TableRow -> [Transition]
getTransitions (TableRow _ _ x _) = x

getPreviousClass :: TableRow -> String
getPreviousClass (TableRow _ _ _ x) = x

getTransitionFromClass :: Transition -> DKAState
getTransitionFromClass (Transition x _ _ _) = x

getTransitionToState :: Transition -> DKAState
getTransitionToState (Transition _ x _ _) = x

getTransitionSymbol :: Transition -> InputSymbol
getTransitionSymbol (Transition _ _ x _) = x

getTransitionToClass :: Transition -> String
getTransitionToClass (Transition _ _ _ x) = x

-- printing functions
instance Show TableRow where
    show TableRow{..} = unwords $ ["\nClass: " ++ tableClass ++ ", State: " ++ state ++ ", Transitions: \n"] ++ map show transitions ++ ["Previous class: " ++ previousClass ++ "\n"]

instance Show Transition where
    show Transition{..} = "class: " ++ fromClass ++ " --" ++ show tSymbol ++ "-> " ++ tToState ++ ", class:" ++ toClass ++ "\n"

-- comparing functions
instance Eq TableRow where
    (TableRow x1 y1 w1 z1) == (TableRow x2 y2 w2 z2) = x1 == x2 && y1 == y2 && w1 == w2 && z1 == z2

instance Eq Transition where
    (Transition x1 y1 w1 z1) == (Transition x2 y2 w2 z2) = x1 == x2 && y1 == y2 && w1 == w2 && z1 == z2
