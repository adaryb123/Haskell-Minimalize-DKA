{-# LANGUAGE RecordWildCards #-}

module MyConversion where

import Data.Set (Set, toList, fromList)
import MyData


convertToMKA :: DKA -> DKA
convertToMKA dka@DKA{..} = removeUnreachableStates (addSINKState dka)