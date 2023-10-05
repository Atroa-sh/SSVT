module Exercise2 where

import Exercise1
import Data.List
import Mutation
import Debug.Trace
import Test.QuickCheck
import MultiplicationTable
import FitSpec

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
countSurvivors n = countSurvivors' n 0

countSurvivors' :: Integer -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
countSurvivors' n current _ _ _
    | n <= 0 = return current
countSurvivors' n current props func mutators = do
    rand <- generate (chooseInteger (1, 10))
    let gen = map (\mutator -> mutate' mutator props func rand) mutators
    results <- mapM generate gen
    let survivors = foldl (\i v -> if v then i + 1 else i) 0 (map or results)
    countSurvivors' (n-genericLength mutators) (survivors+current) props func mutators

