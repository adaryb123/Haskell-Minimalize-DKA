{-# LANGUAGE RecordWildCards, TupleSections #-}

module MyData where

import Control.Arrow (first)
import Control.Monad (join)
import Data.List (intercalate, dropWhileEnd, unfoldr, (\\))
import Data.Set (Set, toList, fromList)

data DKA = DKA {states :: [DKAState],
                alphabet :: [InputSymbol],
                startState :: DKAState,
                endStates :: [DKAState],
                transitions :: Set Rule}

data Rule = Rule {fromState :: DKAState
                 , symbol :: InputSymbol
                 , toState :: DKAState} deriving (Ord)

type Err = Either String
type DKAState = String
type InputSymbol = Char

globalDKA = DKA{states = ["1","5","8","11", "12"]
,   alphabet = ['c','d','e','x']
,   startState = "5"
,   endStates = ["8"]
,   transitions = fromList [Rule{fromState = "5", symbol = 'd', toState = "8"},
                       Rule{fromState = "8", symbol = 'x', toState = "1"},
                       Rule{fromState = "8", symbol = 'c', toState = "5"},
                       Rule{fromState = "11", symbol = 'c', toState = "12"}]}

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

ruleExistsByStartAndSymbol :: Set Rule -> DKAState -> InputSymbol -> Bool
ruleExistsByStartAndSymbol rules' state' symbol' = any (\someRule -> getRuleFromState someRule == state' && getRuleSymbol someRule == symbol') rules'

instance Show DKA where
    show DKA{..} = unwords $ ["Printing DKA:\nStates: " ++ intercalate "," states ++ "\nAlphabet: " ++ alphabet ++ "\nStarting state: " ++ startState ++ "\nEnding states: " ++ intercalate "," endStates ++"\nTransitions:\n"] ++ map show (toList transitions)
 
instance Show Rule where
    show Rule{..} = fromState ++ "--" ++ show symbol ++ "-> " ++ toState ++ "\n"
        
instance Eq Rule where
        (Rule x1 y1 z1) == (Rule x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2



addSINKState :: DKA -> DKA
addSINKState dka@DKA{..} =
    DKA{states = getStates dka ++ ["SINK"]
    ,   alphabet = getAlphabet dka
    ,   startState = getStartingState dka
    ,   endStates = getEndingStates dka
    ,   transitions = newTransitions}
    where newTransitions = fromList (toList (getRules dka) ++ sinkTransitions ((getStates dka) ++ ["SINK"]) (getAlphabet dka) (getRules dka))

sinkTransitionsForState :: DKAState -> [InputSymbol] -> Set Rule -> [Rule]
sinkTransitionsForState _ [] _ = []
sinkTransitionsForState state' (symbol':symbols') rules'
    | ruleExistsByStartAndSymbol rules' state' symbol' = sinkTransitionsForState state' symbols' rules'
    | otherwise = sinkTransitionsForState state' symbols' rules' ++ [Rule{fromState = state', symbol = symbol',toState = "SINK"}]

sinkTransitions :: [DKAState] -> [InputSymbol] -> Set Rule -> [Rule]
sinkTransitions [] _ _ = []
sinkTransitions (state':states') symbols' rules' = sinkTransitionsForState state' symbols' rules' ++ sinkTransitions states' symbols' rules'


findReachableStatesFromStates :: Set Rule -> [DKAState] -> [DKAState]
findReachableStatesFromStates rules' fromStates' =  map getRuleToState (filter (\someRule -> getRuleFromState someRule `elem` fromStates' && getRuleToState someRule `notElem` fromStates') (toList rules'))

findReachableStates :: Set Rule -> [DKAState] -> [DKAState]
findReachableStates _ [] = []
findReachableStates rules' foundStates' 
    | null newStates = foundStates'
    | otherwise = findReachableStates rules' (newStates ++ foundStates')
    where newStates = findReachableStatesFromStates rules' foundStates'


findUnreachableRules :: Set Rule -> [DKAState] -> [Rule]
findUnreachableRules rules' unreachableStates' = filter (\someRule -> getRuleFromState someRule `elem` unreachableStates') ( toList rules')
    -- any (\someRule -> getRuleFromState someRule == state' && getRuleSymbol someRule == symbol') rules'
    

removeUnreachableStates :: DKA -> DKA
removeUnreachableStates dka@DKA{..} =
    DKA{states = (getStates dka) \\ unreachableStates
    ,   alphabet = getAlphabet dka
    ,   startState = getStartingState dka
    ,   endStates = getEndingStates dka
    ,   transitions = fromList ((toList (getRules dka)) \\ (findUnreachableRules (getRules dka) unreachableStates)) }
    where unreachableStates = getStates dka \\ findReachableStates (getRules dka) [getStartingState dka]
