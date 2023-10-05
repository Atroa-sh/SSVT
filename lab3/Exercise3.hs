{-# LANGUAGE BlockArguments #-}
module Exercise3 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- Implement a function that calculates the minimal property subsets given a function under test and a set of properties
minimalPropertySubsets :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool]
minimalPropertySubsets _ _ = undefined

-- In order to do this, we need to be able to keep track of what mutants are being killed / not being killed by every property.
-- Then we can compare the killing performance of every property in the set of properties to filter the properties
-- that are not minimal properties

-- To make sure that all the properties are tested with the same mutants,
-- we need to generate one set of mutants and remember it for the entire execution of the recursive process.

-- Generate a list of n mutants given a list of mutators, a FUT, and an input for the FUT.
getMutations :: Integer -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> Integer -> Gen [[Integer]]
getMutations n muts fut input = do
    -- rand <- generate (chooseInteger (1, 10))
    let output = fut input
    -- let n = n - length muts
    return (getMutations (n - length muts) muts fut input) ++ mapM (\x -> x output) muts

determineSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors n = determineSurvivors' n []

determineSurvivors' :: Integer -> [[Bool]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors' n current _ _ _
    | n <= 0 = return current
determineSurvivors' n current props func mutators = do
    rand <- generate (chooseInteger (1, 10))
    let gen = map (\mutator -> mutate' mutator props func rand) mutators
    results <- mapM generate gen
    -- let survivors = foldl (\i v -> if v then i + 1 else i) 0 (map or results)
    determineSurvivors' (n-genericLength mutators) (results++current) props func mutators