module Exercise3 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- This is the main function of this exercise.
-- By running it with a number of mutants, a list of properties, a function and a list of mutators it will return the index of the most minimal property.
-- This index is equal to the index of the property in the list of properties.
minimalPropertySubsets :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Int
minimalPropertySubsets n props func mutators = do
    survivors <- determineSurvivors n props func mutators
    determineSubsets survivors

-- Helper function to get a list of n mutators given a list of mutators and a number n.
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

-- Helper function to execute a property given a list of properties, a mutant and an input for the FUT.
determineSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors n = determineSurvivors' n []

-- Helper function to execute a property given a list of properties, a mutant and an input for the FUT.
determineSurvivors' :: Integer -> [[Bool]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [[Bool]]
determineSurvivors' n current props func mutators = do
        rand <- generate (chooseInteger (1, 10))
        mutations <- generate $ getMutations n mutators func rand
        let gen = map (\mutant -> propertyExecutor' props mutant rand) mutations
        mapM generate gen

-- Helper function to check if a property is a subset or equal to another property.
-- If prop1 element is false && prop2 element is true -> Continue
-- If prop1 element is false && prop2 element is false -> Continue
-- If prop1 element is true && prop2 element is true -> Continue
-- If prop1 element is true && prop2 element is false -> Break
isSubOrEquiv :: [Bool] -> [Bool] -> Bool
isSubOrEquiv prop1 prop2 = all (\(x, y) -> not x && y || not (x && not y)) (zip prop1 prop2)

-- Helper function to determine the most minimal property given a list of properties.
determineSubsets :: [[Bool]] -> IO Int
determineSubsets prop = do
    let allProps = transpose prop
    let results = map (\y -> map (`isSubOrEquiv` y) allProps) allProps
    -- Return the index of the list which has the most False values in it, which is the most minimal property
    let maxIndex = case elemIndex (maximum $ zipWith (curry (length . filter not . snd)) [1..] results) (zipWith (curry (length . filter not . snd)) [1..] results) of
                        Just i -> i
                        Nothing -> error "No maximum element found"
    return $ maxIndex + 1

