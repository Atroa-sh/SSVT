module Exercise2 where

import Exercise1
import Data.List
import Mutation
import Test.QuickCheck
import FitSpec

countSurvivors :: Integer -> [[Integer] -> Integer -> Property] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> Integer
-- countSurvivors n props func = sum $ map (\x -> if x then 1 else 0)
-- countSurvivors n props func mutators = do
--     -- Generate N amount of mutated lists
--     mutatedInputs <- mapM (\x -> mutate x props func 10) mutators
--     -- Map the function on the mutated inputs
--     mutatedOutputs <- mapM (return . func) mutatedInputs

--     -- Count how many mutatedInputs mapped on the function do not fulfill property
--     -- return that value
countSurvivors n props func mutators = countSurvivors n 0 props func mutators

countSurvivors :: Integer -> Integer -> [[Integer] -> Integer -> Property] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> Integer
countSurvivors' 0 current props func mutators = current
countSurvivors' n current props func mutators = do
    gen = map (\mutator -> mutate' mutator MultiplicationTable.multiplicationTableProps MultiplicationTable.multiplicationTable 5) mutators
    --mapM generate gen gives us [[False,False,False,False,False],[False,True,False,True,True],[False,False,False,False,False]]
    survivors = if orConcat (map () mutators)
    return countSurvivors' (n-1 survivors props func mutators)

orConcat :: [Bool] -> Bool
orConcat [] = False
orConcat (x:xs) = if x then True else orConcat xs

