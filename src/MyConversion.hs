-- Project : FLP 2 DKA-2-MKA
-- Author : Adam Rybanský
-- Login : xryban00

{-# LANGUAGE RecordWildCards #-}

module MyConversion where

import Data.List ((\\))
import Data.Set (Set, toList, fromList)
import MyData

-- /////////////////////////////////////////////////////////////////////////////////////
--this block of functions adds the SINK state to DKA (addSINKState is the main function)

ruleExistsByStartAndSymbol :: Set Rule -> DKAState -> InputSymbol -> Bool
ruleExistsByStartAndSymbol rules' state' symbol' = any (\someRule -> getRuleFromState someRule == state' && getRuleSymbol someRule == symbol') (toList rules')

addSINKState :: DKA -> DKA
addSINKState dka@DKA{..} =
    DKA{states = getStates dka ++ ["SINK"]
    ,   alphabet = getAlphabet dka
    ,   startState = getStartingState dka
    ,   endStates = getEndingStates dka
    ,   rules = newRules}
    where newRules = fromList (toList (getRules dka) ++ sinkRules (getStates dka ++ ["SINK"]) (getAlphabet dka) (getRules dka))

sinkRulesForState :: DKAState -> [InputSymbol] -> Set Rule -> [Rule]
sinkRulesForState _ [] _ = []
sinkRulesForState state' (symbol':symbols') rules'
    | ruleExistsByStartAndSymbol rules' state' symbol' = sinkRulesForState state' symbols' rules'
    | otherwise = sinkRulesForState state' symbols' rules' ++ [Rule{fromState = state', symbol = symbol',toState = "SINK"}]

sinkRules :: [DKAState] -> [InputSymbol] -> Set Rule -> [Rule]
sinkRules [] _ _ = []
sinkRules (state':states') symbols' rules' = sinkRulesForState state' symbols' rules' ++ sinkRules states' symbols' rules'

-- ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--this block of functions removes the unreachable states from the DKA (removeUnreachableStates is the main function)
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
    DKA{states = getStates dka \\ unreachableStates
    ,   alphabet = getAlphabet dka
    ,   startState = getStartingState dka
    ,   endStates = getEndingStates dka
    ,   rules = fromList (toList (getRules dka) \\ findUnreachableRules (getRules dka) unreachableStates) }
    where unreachableStates = getStates dka \\ findReachableStates (getRules dka) [getStartingState dka]

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--this block of functions converts DKA to AlgorithmTable datatype, which is better required for the minimalization algorithm (createTable is the main function)
convertDKAtoTable :: DKA -> AlgorithmTable
convertDKAtoTable dka@DKA{..} = setTransitionsAccordingToClass table table
        where table = createFirstRowForStartingState (getRules dka) (getStartingState dka) (getEndingStates dka) : createTable (getRules dka) (getStates dka) (getEndingStates dka)

createFirstRowForStartingState ::  Set Rule -> DKAState -> [DKAState] -> TableRow
createFirstRowForStartingState = createTableRow

createTable :: Set Rule -> [DKAState] -> [DKAState] -> AlgorithmTable
createTable _ [] _ = []
createTable rules' (state':states') endingStates' = createTableRow rules' state' endingStates' : createTable rules' states' endingStates'

createTableRow :: Set Rule -> DKAState -> [DKAState] -> TableRow
createTableRow rules' state' endingStates' =
    TableRow{tableClass = calculatedClass
    ,   state = state'
    ,   transitions = extractTransitions rules' state'
    ,   previousClass = calculatedClass}
    where calculatedClass = determineClass state' endingStates'

determineClass :: DKAState -> [DKAState] -> String
determineClass state' endingStates'
    | state' `elem` endingStates' = "2"
    | otherwise = "1"

extractTransitions :: Set Rule -> DKAState -> [Transition]
extractTransitions rules' state' = map convertRuleToTransition (filter (\someRule -> getRuleFromState someRule == state') ( toList rules'))

convertRuleToTransition :: Rule -> Transition
convertRuleToTransition rule@Rule{..} =
    Transition{fromClass = "0",
                tToState = getRuleToState rule,
                tSymbol = getRuleSymbol rule,
                toClass = "0"}

-- /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--the minimalisation algorithm consists of 3 steps:
--first step is adjusting the classes of the states in transitions in each row (setTransitions is the main function)

setTransitionsAccordingToClass :: [TableRow]  -> [TableRow] -> [TableRow]
setTransitionsAccordingToClass [] _ = []
setTransitionsAccordingToClass (row':rows') table' = setTransitions row' table' : setTransitionsAccordingToClass rows' table'

getTableRowClassFromTable :: [TableRow] -> DKAState -> String
getTableRowClassFromTable [] _ = "No class found"
getTableRowClassFromTable (row':rows') state'
    | getState row' == state' = getClass row'
    | otherwise = getTableRowClassFromTable rows' state'

setTransitions :: TableRow -> AlgorithmTable -> TableRow
setTransitions row' table' = row'{transitions = changedTransitions}
        where changedTransitions = map helper (getTransitions row')
              helper x = setClass x (getClass row') (getTableRowClassFromTable table' (getTransitionToState x))

setClass :: Transition -> String -> String -> Transition
setClass transition' fromClass' toClass' = transition'{fromClass = fromClass', toClass = toClass'}

-- ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
--the second step is creating new table object based on the previous one, and further dividing the classes by the classes in their transitions.
-- (formTableWithNewClasses is the main function) 

findHighestClass :: [TableRow] -> String
findHighestClass [] = "0"
findHighestClass (row':rows') = max string1 string2
    where string1 = getClass row'
          string2 = findHighestClass rows'

transitionClassesEqual :: [Transition] -> [Transition] -> Bool
transitionClassesEqual [] [] = True
transitionClassesEqual [] _ = False
transitionClassesEqual _ [] = True
transitionClassesEqual (tran1:trans1) (tran2:trans2) = getTransitionToClass tran1 == getTransitionToClass tran2 && transitionClassesEqual trans1 trans2

rowsBelongToSameClass :: TableRow -> TableRow -> Bool
rowsBelongToSameClass row1' row2' = getPreviousClass row1' == getPreviousClass row2' && transitionClassesEqual (getTransitions row1') (getTransitions row2')

calculateTableClass :: TableRow -> [TableRow] -> [TableRow] -> String
calculateTableClass _ [] newTable' =  show ((read (findHighestClass newTable') :: Integer)  + 1)
calculateTableClass row' (newRow':newRows') newTable'
    | rowsBelongToSameClass row' newRow' = getClass newRow'
    | otherwise = calculateTableClass row' newRows' newTable'

formTableWithNewClasses :: [TableRow] -> [TableRow] -> [TableRow]
formTableWithNewClasses [] _ = []
formTableWithNewClasses (oldRow':oldRows') [] = newRow : formTableWithNewClasses oldRows' [newRow]
        where newRow = TableRow{tableClass = "0", state = getState oldRow', transitions = getTransitions oldRow', previousClass = getPreviousClass oldRow'}
formTableWithNewClasses (oldRow':oldRows') newTable' = newRow : formTableWithNewClasses oldRows' (newTable' ++ [newRow])
        where newRow = TableRow{tableClass = calculateTableClass oldRow' newTable' newTable', state = getState oldRow', transitions = getTransitions oldRow', previousClass = getPreviousClass oldRow'}

-- ///////////////////////////////////////////////////////////////////////////
-- the last step is setting the previous class variable for each current class
-- (setPreviousClasses is the main function)

findRowByState :: [TableRow] -> DKAState -> TableRow
findRowByState [] _ = TableRow{tableClass = "none", state = "none", transitions = [], previousClass = "none"}
findRowByState (row':rows') state'
    | getState row' == state' = row'
    | otherwise = findRowByState rows' state'

setPreviousClass :: TableRow -> String -> TableRow
setPreviousClass row' class' = row'{previousClass = class'}

setPreviousClasses :: [TableRow] -> [TableRow] -> [TableRow]
setPreviousClasses oldTable' = map helper
    where helper x = setPreviousClass x (getClass (findRowByState oldTable' (getState x)))

-- ///////////////////////////////////////////////////////////////////////////////////////////////
-- we run the algorithm 1 step at a time, until the current table and the previous table are equal
-- (that means the classes cannot further be divided) 

perform1Step :: AlgorithmTable -> AlgorithmTable
perform1Step table' = setTransitionsAccordingToClass halfResult halfResult
        where halfResult = setPreviousClasses table' (formTableWithNewClasses table' [])

runAlgorithm :: AlgorithmTable -> AlgorithmTable
runAlgorithm oldtable
    | newtable == oldtable = newtable
    | otherwise = runAlgorithm newtable
    where newtable = perform1Step oldtable

-- ///////////////////////////////////////////////////////////////////////
-- these functions convert the final table to DKA, using classes as states
-- (convertTabletoDKA is the main function)

getClassesFromTable :: AlgorithmTable -> [String]
getClassesFromTable = map getClass

getClassesThatContainStates :: AlgorithmTable -> [DKAState] -> [String]
getClassesThatContainStates [] _ = []
getClassesThatContainStates (row':rows') states'
    | getState row' `elem` states' = getClass row' : getClassesThatContainStates rows' states'
    | otherwise = getClassesThatContainStates rows' states'

convertTransitionsToRules :: [Transition] -> [Rule]
convertTransitionsToRules = map
      (\ tran'
         -> Rule
              {fromState = getTransitionFromClass tran',
               symbol = getTransitionSymbol tran',
               toState = getTransitionToClass tran'})

convertAllTransitionsInTable :: AlgorithmTable -> [Rule]
convertAllTransitionsInTable = concatMap (convertTransitionsToRules . getTransitions)

convertTableToDKA :: AlgorithmTable -> DKA -> DKA
convertTableToDKA  table inputDKA@DKA{..} =
    DKA{states = toList (fromList (getClassesFromTable table))
    ,   alphabet = getAlphabet inputDKA
    ,   startState = head (getClassesThatContainStates table [getStartingState inputDKA])
    ,   endStates = toList (fromList (getClassesThatContainStates table (getEndingStates inputDKA)))
    ,   rules = fromList (convertAllTransitionsInTable table)}

-- ////////////////////////////////////////////////
--function calling the entire minimalization process

convertToMKA :: DKA -> DKA
convertToMKA dka@DKA{..} = convertTableToDKA (runAlgorithm ( convertDKAtoTable ( removeUnreachableStates (addSINKState dka)))) dka