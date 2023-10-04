module Exercise2 where

import Exercise1
import Data.List
import Mutation
import Test.QuickCheck
import FitSpec

countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> Integer
-- countSurvivors n props func = sum $ map (\x -> if x then 1 else 0)
-- countSurvivors n props func mutators = do
--     -- Generate N amount of mutated lists
--     mutatedInputs <- mapM (\x -> mutate x props func 10) mutators
--     -- Map the function on the mutated inputs
--     mutatedOutputs <- mapM (return . func) mutatedInputs

--     -- Count how many mutatedInputs mapped on the function do not fulfill property
--     -- return that value
countSurvivors n props func mutators = map




