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

getListOfMutators :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Gen [Integer]]
getListOfMutators muts n
    | n > 0 = genericTake n (muts ++ getListOfMutators muts (n - fromIntegral (length muts)))
    | otherwise = []

-- Generate a list of n mutants given a list of mutators, a FUT, and an input for the FUT.
getMutations :: Integer -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> Integer -> Gen [[Integer]]
getMutations n muts fut input = do
    let output = fut input
    let listOfMutators = getListOfMutators muts n
    mapM (\x -> x output) listOfMutators

determineSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors n = determineSurvivors' n []

determineSurvivors' :: Integer -> [[Bool]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors' n current props func mutators = do
        rand <- generate (chooseInteger (1, 10))
        mutations <- generate $ getMutations n mutators func rand
        let gen = map (\mutant -> propertyExecutor' props mutant rand) mutations
        mapM generate gen

-- Define what it means for a property to be a subset of another property given both of their outcomes.
-- That way we can double loop over all the property outcomes

-- These are the cases where we would write down the second input as subset of the first, so we want to remember the first input as a minimal property.
isSubOrEquiv :: [Bool] -> [Bool] -> Bool
isSubOrEquiv prop1 prop2 = prop1 == prop2
    -- Loop over the elements in both lists:
        -- If prop1 element is false && prop2 element is true -> Continue
        -- If prop1 element is false && prop2 element is false -> Continue
        -- If prop1 element is true && prop2 element is true -> Continue
        -- If prop1 element is true && prop2 element is false -> Break

determineSubsets :: [[Bool]] -> IO [[Bool]]
determineSubsets prop = do
    let allProps = transpose prop
    return $ map (\y -> map (`isSubOrEquiv` y) allProps) allProps

