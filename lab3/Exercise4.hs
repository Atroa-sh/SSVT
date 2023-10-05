{- Time spent: 1 hour -}

module Exercise4 where

import Exercise2
import Data.List
import Test.QuickCheck
import Data.Function
import Mutation
import MultiplicationTable

-- This function calculates the strenght of a set of properties.
-- Each property is individually tested on request of the TA, it was not completely clear from the assignment.
-- The function takes the same arguments as countSurvivors, but returns a Float instead of an Integer.
strength :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO [Float]
strength n props func mutators = do
    mapM (\prop -> strength' n prop func mutators) props

-- This function calculates the strength of one property.
-- The strength is calculated by dividing the number of killed mutants by the total number of mutants.
strength' :: Integer -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Float
strength' n prop func mutators = do
    survivors <- countSurvivors n [prop] func mutators
    return $ (fromIntegral (n-survivors) / fromIntegral n) * 100
