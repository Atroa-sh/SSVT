module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1
import Exercise3
import Exercise5

-- Test the functions symClos and trClos from the previous exercises. Devise your own test
-- method for this. Try to use random test generation. Define reasonable properties to test. Can
-- you use QuickCheck? How?

-- Function to check if a relation is symmetric
prop_isSymmetric :: Eq a => Rel a -> Bool
prop_isSymmetric [] = True
prop_isSymmetric rel = all (\(x, y) -> (y, x) `elem` rel) rel

-- Function to check if a relation is transitive
prop_isTransitive :: Eq a => Rel a -> Bool
prop_isTransitive [] = True
prop_isTransitive rel = all (\(x, y) -> all (\(a, b) -> y /= a || (x, b) `elem` rel) rel) rel