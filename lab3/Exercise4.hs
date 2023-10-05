{- Time spent: 30 minutes -}

module Exercise4 where

import Exercise2
import Data.List
import Test.QuickCheck
import Data.Function
import Mutation
import MultiplicationTable

-- This function calculates the strenght of a set of properties.
-- The strength is calculated by dividing the number of killed mutants by the total number of mutants.
-- The function takes the same arguments as countSurvivors, but returns a Float instead of an Integer.
-- This is done because we need the amount of mutants, the set of properties, the function to test it on and the mutants that we want to test.
strength :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Float
strength n props func mutators = do
    survivors <- countSurvivors n props func mutators
    return $ (fromIntegral (n-survivors) / fromIntegral n) * 100
