module Exercise3 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Debug.Trace (trace)

{--
    Time Spent: 20 hours

    The approach we took was to rewrite the function we made for exercise 2 to output
    the list of outputs of every porperty and mutation combination. This way
    we can use this output to compare the outputs of two properties. We can then see
    whether a single property is a subset of another porperty based on the outputs
    of the mutations. In order to do this we had to make sure that all properties make
    use of the same mutations when the determineSurvivors function is executed.

    There is a flaw in the implementation: The determineSubset function does not see equivalent as minimal subsets.
    This is due to the way we check if a property is a subset of another property.
    We check whether one property kills the same mutants as another property and add it to the list of minimal properties
    if it does not. Due to this, we filter the properties that are equivalent to another property,
    because they kill the same mutants. This is not the desired behaviour, but we were unable to fix this.

    We tried numerous approaches, including building the minimal subsets one by one, removing elements if they end up
    being subsets of others or equivalent, but we were unable to implement this.
--}

-- This is the main function of this exercise.
-- By running it with a number of mutants, a list of properties, a function and a list of mutators it will return the index of the most minimal property.
-- This index is equal to the index of the property in the list of properties.
minimalPropertySubsets :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO [Int]
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

-- Returns false if the property kills unique mutants. If True is returned, the property is not a minimal property.
checkOccurrences :: [Bool] -> [[Bool]] -> Bool
checkOccurrences firstInput = any (all (\(x, y) -> x || x == y) . zip firstInput)

-- Helper function to determine the most minimal property given a list of properties.
determineSubsets :: [[Bool]] -> IO [Int]
determineSubsets prop = do
    let allProps = transpose prop
    -- Call checkOccurences on every element of allProps with the rest of allProps as input, excluding itself.
    -- If the result is True, the property is not a minimal property.
    let results = map (\x -> checkOccurrences x (delete x allProps)) allProps
    -- For each element that is false, return the index.
    let falseIndices = elemIndices False results
    return $ map (1+) falseIndices

