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
                rules :: Set Rule}

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
,   rules = fromList [Rule{fromState = "5", symbol = 'd', toState = "8"},
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
    show DKA{..} = unwords $ ["Printing DKA:\nStates: " ++ intercalate "," states ++ "\nAlphabet: " ++ alphabet ++ "\nStarting state: " ++ startState ++ "\nEnding states: " ++ intercalate "," endStates ++"\nRules:\n"] ++ map show (toList rules)
 
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
    ,   rules = newRules}
    where newRules = fromList (toList (getRules dka) ++ sinkRules ((getStates dka) ++ ["SINK"]) (getAlphabet dka) (getRules dka))

sinkRulesForState :: DKAState -> [InputSymbol] -> Set Rule -> [Rule]
sinkRulesForState _ [] _ = []
sinkRulesForState state' (symbol':symbols') rules'
    | ruleExistsByStartAndSymbol rules' state' symbol' = sinkRulesForState state' symbols' rules'
    | otherwise = sinkRulesForState state' symbols' rules' ++ [Rule{fromState = state', symbol = symbol',toState = "SINK"}]

sinkRules :: [DKAState] -> [InputSymbol] -> Set Rule -> [Rule]
sinkRules [] _ _ = []
sinkRules (state':states') symbols' rules' = sinkRulesForState state' symbols' rules' ++ sinkRules states' symbols' rules'


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

removeUnreachableStates :: DKA -> DKA
removeUnreachableStates dka@DKA{..} =
    DKA{states = (getStates dka) \\ unreachableStates
    ,   alphabet = getAlphabet dka
    ,   startState = getStartingState dka
    ,   endStates = getEndingStates dka
    ,   rules = fromList ((toList (getRules dka)) \\ (findUnreachableRules (getRules dka) unreachableStates)) }
    where unreachableStates = getStates dka \\ findReachableStates (getRules dka) [getStartingState dka]











data TableRow = TableRow{tableClass :: String
    , state :: DKAState
    , transitions :: [Transition]
}

data Transition = Transition{tFromState :: DKAState
    , tToState :: DKAState
    , tSymbol :: InputSymbol
    , toClass :: String
} deriving (Ord, Eq)

type AlgorithmTable = [TableRow]

getClass :: TableRow -> String
getClass (TableRow x _ _) = x

getState :: TableRow -> DKAState
getState (TableRow _ x _) = x

getTransitions :: TableRow -> [Transition]
getTransitions (TableRow _ _ x) = x

getTransitionFromState :: Transition -> DKAState
getTransitionFromState (Transition x _ _ _) = x

getTransitionSymbol :: Transition -> InputSymbol
getTransitionSymbol (Transition _ _ x _) = x

getTransitionToClass :: Transition -> String
getTransitionToClass (Transition _ _ _ x) = x


instance Show TableRow where
    show TableRow{..} = unwords $ ["\nClass: " ++ tableClass ++ ", State: " ++ state ++ ", Transitions: \n"] ++ map show transitions
 
instance Show Transition where
    show Transition{..} = tFromState ++ "--" ++ show tSymbol ++ "-> " ++ tToState ++ ", class:" ++ toClass ++ "\n"
    
-- instance Eq Rule where
--         (Rule x1 y1 z1) == (Rule x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2


convertDKAtoTable :: DKA -> AlgorithmTable
convertDKAtoTable dka@DKA{..} = createTable (getRules dka) (getStates dka) (getEndingStates dka)

createTable :: Set Rule -> [DKAState] -> [DKAState] -> AlgorithmTable
createTable _ [] _ = []
createTable rules' (state':states') endingStates' = (createTableRow rules' state' endingStates') : (createTable rules' states' endingStates')

createTableRow :: Set Rule -> DKAState -> [DKAState] -> TableRow
createTableRow rules' state' endingStates' = 
    TableRow{tableClass = determineClass state' endingStates'
    ,   state = state'
    ,   transitions = extractTransitions rules' state' (determineClass state' endingStates')}

determineClass :: DKAState -> [DKAState] -> String
determineClass state' endingStates' 
    | state' `elem` endingStates' = "1"
    | otherwise = "2"

extractTransitions :: Set Rule -> DKAState -> String -> [Transition]
extractTransitions rules' state' class' = map convertRuleToTransition (filter (\someRule -> getRuleFromState someRule == state') ( toList rules'))

convertRuleToTransition :: Rule -> Transition
convertRuleToTransition rule@Rule{..} =
    Transition{tFromState = getRuleFromState rule,
               tToState = getRuleToState rule,
               tSymbol = getRuleSymbol rule,
               toClass = "0"}