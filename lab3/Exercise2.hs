module Exercise2 where

import Exercise1
import Data.List
import Mutation
import Debug.Trace
import Test.QuickCheck
import MultiplicationTable
import FitSpec

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
-- countSurvivors n props func = sum $ map (\x -> if x then 1 else 0)
-- countSurvivors n props func mutators = do
--     -- Generate N amount of mutated lists
--     mutatedInputs <- mapM (\x -> mutate x props func 10) mutators
--     -- Map the function on the mutated inputs
--     mutatedOutputs <- mapM (return . func) mutatedInputs

--     -- Count how many mutatedInputs mapped on the function do not fulfill property
--     -- return that value
countSurvivors n props func mutators = countSurvivors' n 0 props func mutators

countSurvivors' :: Integer -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
countSurvivors' n current _ _ _ 
    | n <= 0 = return current
countSurvivors' n current props func mutators = do
    rand <- generate (chooseInt (1, 10))
    let gen = map (\mutator -> mutate' mutator props func (toInteger $ rand)) mutators
    --mapM generate gen gives us [[False,False,False,False,False],[False,True,False,True,True],[False,False,False,False,False]]
    let tmp = mapM generate gen
    results <- tmp
    trace (show results) $ return results
    let survivors = foldl (\i v -> if v then i + 1 else i) 0 (map orConcat results)
    countSurvivors' (n-(toInteger $ length mutators)) (survivors+current) props func mutators

orConcat :: [Bool] -> Bool
orConcat [] = False
orConcat (x:xs) = if x then True else orConcat xs

