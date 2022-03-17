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

-- globalDKA = DKA{states = ["1","5","8","11", "12"]
-- ,   alphabet = ['c','d','e','x']
-- ,   startState = "5"
-- ,   endStates = ["8"]
-- ,   rules = fromList [Rule{fromState = "5", symbol = 'd', toState = "8"},
--                        Rule{fromState = "8", symbol = 'x', toState = "1"},
--                        Rule{fromState = "8", symbol = 'c', toState = "5"},
--                        Rule{fromState = "11", symbol = 'c', toState = "12"}]}

-- globalTableRow1 = TableRow{tableClass = "1", state = "1", transitions =
--     [Transition{tFromState = "1", tSymbol = 'a', tToState = "7", toClass = "2"},
--      Transition{tFromState = "7", tSymbol = 'a', tToState = "7", toClass = "2"},
--      Transition{tFromState = "6", tSymbol = 'a', tToState = "1", toClass = "1"}]}

-- globalTableRow2 = TableRow{tableClass = "1", state = "5", transitions =
-- [Transition{tFromState = "5", tSymbol = 'b', tToState = "7", toClass = "2"},
--     Transition{tFromState = "7", tSymbol = 'b', tToState = "7", toClass = "2"},
--     Transition{tFromState = "10", tSymbol = 'a', tToState = "7", toClass = "2"}]}
    
-- globalTableRow3 = TableRow{tableClass = "1", state = "6", transitions =
--     [Transition{tFromState = "5", tSymbol = 'b', tToState = "7", toClass = "2"},
--         Transition{tFromState = "7", tSymbol = 'b', tToState = "7", toClass = "2"},
--         Transition{tFromState = "10", tSymbol = 'a', tToState = "7", toClass = "2"}]}

-- globalTable1 = [globalTableRow1,globalTableRow2]
-- globalTable2 = [globalTableRow2,globalTableRow1]

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
    , previousClass :: String
}

data Transition = Transition{tFromState :: DKAState
    , tToState :: DKAState
    , tSymbol :: InputSymbol
    , toClass :: String
} deriving (Ord)

type AlgorithmTable = [TableRow]

getClass :: TableRow -> String
getClass (TableRow x _ _ _) = x

getState :: TableRow -> DKAState
getState (TableRow _ x _ _) = x

getTransitions :: TableRow -> [Transition]
getTransitions (TableRow _ _ x _) = x

getPreviousClass :: TableRow -> String
getPreviousClass (TableRow _ _ _ x) = x

getTransitionFromState :: Transition -> DKAState
getTransitionFromState (Transition x _ _ _) = x

getTransitionToState :: Transition -> DKAState
getTransitionToState (Transition _ x _ _) = x

getTransitionSymbol :: Transition -> InputSymbol
getTransitionSymbol (Transition _ _ x _) = x

getTransitionToClass :: Transition -> String
getTransitionToClass (Transition _ _ _ x) = x


instance Show TableRow where
    show TableRow{..} = unwords $ ["\nClass: " ++ tableClass ++ ", State: " ++ state ++ ", Transitions: \n"] ++ map show transitions ++ ["Previous class: " ++ previousClass ++ "\n"] 
 
instance Show Transition where
    show Transition{..} = tFromState ++ "--" ++ show tSymbol ++ "-> " ++ tToState ++ ", class:" ++ toClass ++ "\n"
    

-- instance Eq AlgorithmTable where
--     (AlgorithmTable []) == (AlgorithmTable []) = True
--     (AlgorithmTable rows1) == (AlgorithmTable []) = False
--     (AlgorithmTable []) == (AlgorithmTable rows2) = False
--     (AlgorithmTable row1:rows1) == (AlgorithmTable row2:rows2) = row1 == row2 && rows1 == rows2


instance Eq TableRow where
    (TableRow x1 y1 w1 z1) == (TableRow x2 y2 w2 z2) = x1 == x2 && y1 == y2 && w1 == w2 && z1 == z2

instance Eq Transition where
    (Transition x1 y1 w1 z1) == (Transition x2 y2 w2 z2) = x1 == x2 && y1 == y2 && w1 == w2 && z1 == z2

convertDKAtoTable :: DKA -> AlgorithmTable
convertDKAtoTable dka@DKA{..} = createTable (getRules dka) (getStates dka) (getEndingStates dka)

createTable :: Set Rule -> [DKAState] -> [DKAState] -> AlgorithmTable
createTable _ [] _ = []
createTable rules' (state':states') endingStates' = (createTableRow rules' state' endingStates') : (createTable rules' states' endingStates')

createTableRow :: Set Rule -> DKAState -> [DKAState] -> TableRow
createTableRow rules' state' endingStates' = 
    TableRow{tableClass = calculatedClass
    ,   state = state'
    ,   transitions = extractTransitions rules' state' calculatedClass
    ,   previousClass = calculatedClass}
    where calculatedClass = determineClass state' endingStates'

determineClass :: DKAState -> [DKAState] -> String
determineClass state' endingStates' 
    | state' `elem` endingStates' = "2"
    | otherwise = "1"

extractTransitions :: Set Rule -> DKAState -> String -> [Transition]
extractTransitions rules' state' class' = map convertRuleToTransition (filter (\someRule -> getRuleFromState someRule == state') ( toList rules'))

convertRuleToTransition :: Rule -> Transition
convertRuleToTransition rule@Rule{..} =
    Transition{tFromState = getRuleFromState rule,
               tToState = getRuleToState rule,
               tSymbol = getRuleSymbol rule,
               toClass = "0"}




setTransitionsAccordingToClass :: [TableRow]  -> [TableRow] -> [TableRow]
setTransitionsAccordingToClass [] _ = []
setTransitionsAccordingToClass (row':rows') table' = [setTransitions row' table'] ++ setTransitionsAccordingToClass rows' table'

getTableRowClassFromTable :: [TableRow] -> DKAState -> String
getTableRowClassFromTable [] _ = "No class found"
getTableRowClassFromTable (row':rows') state' 
    | getState row' == state' = getClass row'
    | otherwise = getTableRowClassFromTable rows' state' 

setTransitions :: TableRow -> AlgorithmTable -> TableRow
setTransitions row' table' = row'{transitions = changedTransitions}
    where changedTransitions = map helper (getTransitions row')
          helper x = setClass x (getTableRowClassFromTable table' (getTransitionToState x))

setClass :: Transition -> String -> Transition
setClass transition' class' = transition'{toClass = class'}

-- findEqualRowClassState :: TableRow -> AlgorithmTable -> (Bool,String)




findHighestClass :: [TableRow] -> String
findHighestClass [] = "1"
findHighestClass (row':rows') = max string1 string2
    where num1 = read string1 :: Integer
          num2 = read string2 :: Integer
          string1 = getClass row'
          string2 = findHighestClass rows'

transitionClassesEqual :: [Transition] -> [Transition] -> Bool
transitionClassesEqual [] [] = True
transitionClassesEqual [] _ = False
transitionClassesEqual _ [] = True
transitionClassesEqual (tran1:trans1) (tran2:trans2) = getTransitionToClass tran1 == getTransitionToClass tran2 && transitionClassesEqual trans1 trans2

rowsBelongToSameClass :: TableRow -> TableRow -> Bool
rowsBelongToSameClass row1' row2' = getPreviousClass row1' == getPreviousClass row2' && transitionClassesEqual (getTransitions row1') (getTransitions row2')

calculateTableClass :: TableRow -> [TableRow] -> [TableRow] -> String
calculateTableClass row' [] newTable' =  show ((read (findHighestClass newTable') :: Integer)  + 1)
calculateTableClass row' (newRow':newRows') newTable'
    | rowsBelongToSameClass row' newRow' = getClass newRow'
    | otherwise = calculateTableClass row' newRows' newTable'


formTableWithNewClasses :: [TableRow] -> [TableRow] -> [TableRow]
formTableWithNewClasses [] _ = []
formTableWithNewClasses (oldRow':oldRows') [] = [newRow] ++ formTableWithNewClasses oldRows' [newRow]
        where newRow = TableRow{tableClass = "1", state = getState oldRow', transitions = getTransitions oldRow', previousClass = getPreviousClass oldRow'}
formTableWithNewClasses (oldRow':oldRows') newTable' = [newRow] ++ formTableWithNewClasses oldRows' (newTable' ++ [newRow])
        where newRow = TableRow{tableClass = (calculateTableClass oldRow' newTable' newTable'), state = getState oldRow', transitions = getTransitions oldRow', previousClass = getPreviousClass oldRow'}

   



findRowByState :: [TableRow] -> DKAState -> TableRow
findRowByState [] _ = TableRow{tableClass = "none", state = "none", transitions = [], previousClass = "none"}
findRowByState (row':rows') state'
    | getState row' == state' = row'
    | otherwise = findRowByState rows' state'


setPreviousClass :: TableRow -> String -> TableRow
setPreviousClass row' class' = row'{previousClass = class'}

setPreviousClasses :: [TableRow] -> [TableRow] -> [TableRow]
setPreviousClasses oldTable' newTable' = map helper newTable'
    where helper x = setPreviousClass x (getClass (findRowByState oldTable' (getState x)))