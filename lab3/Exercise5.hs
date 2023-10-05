module Exercise5 where

import Exercise1
import Exercise3
import Data.List
import Mutation
import Debug.Trace
import Test.QuickCheck
import MultiplicationTable
import FitSpec


--returns whether 2 sets of properties are equal, takes 1st list of properties, 2nd list of properties,
-- function, number of mutants we want to compare functions with, input of the function
areSetsEquivalent :: [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer -> Integer -> Bool
areSetsEquivalent props1 props2 fut muts n input =
    | props1 == props2 = True
areSetsEquivalent props1 props2 fut muts n input = do
    let mutants = getMutations n muts fut input
    let res1 = determineSurvivors props1 fut mutants
    let res2 = determineSurvivors props2 fut mutants
    return res1 == res2

--returns whether the mutation results of 1st set are included in the second one. takes 1st list of properties, 2nd list of properties,
-- function, number of mutants we want to compare functions with, input of the function
isSetImplication :: [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer -> Integer -> Bool
isSetImplication props1 props2 fut muts n input =
    | props1 == props2 = True
isSetImplication props1 props2 fut muts n input = do
    let mutants = getMutations n muts fut input
    let res1 = determineSurvivors props1 fut mutants
    let res2 = determineSurvivors props2 fut mutants
    -- TODO basically we need to check if every single "False" from res1 exists in res2. If yes return true else false
    return res1 == res2