-- Time spent: 2 hours

module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1
import Exercise3
import Exercise5

-- Function to check if a relation is symmetric
-- SymClos should always be symmetric as it is the symmetric closure of a relation
prop_isSymmetric :: Ord a => Rel a -> Bool
prop_isSymmetric [] = True
prop_isSymmetric rel = do
    let sym = symClos rel
    all (\(x, y) -> (y, x) `elem` sym) sym

-- Function to check if a relation is transitive
-- TrClos should always be transitive as it is the transitive closure of a relation
prop_isTransitive :: Ord a => Rel a -> Bool
prop_isTransitive [] = True
prop_isTransitive rel = do
    let tr = trClos rel
    all (\(x, y) -> all (\(a, b) -> y /= a || (x, b) `elem` tr) tr) tr

-- Main function to run all tests
main :: IO ()
main = do
    quickCheck (prop_isSymmetric :: Rel Int -> Bool)
    quickCheck (prop_isTransitive :: Rel Int -> Bool)

{- Test Report:
 For each of the to test functions, one property is tested.
 The first property prop_isSymmetric tests the symClos function.
 It does this by checking if the symmetric closure of a relation is symmetric.
 This is important because by definition the symmetric closure of a relation should be symmetric.
 The second property prop_isTransitive tests the trClos function.
 It does this by checking if the transitive closure of a relation is transitive.
 This is important because by definition the transitive closure of a relation should be transitive.
 All properties are testable with QuickCheck.
 To test the properties, run the following command in ghci:
 ghci> quickCheck (prop_isSymmetric :: Rel Int -> Bool)
 ghci> quickCheck (prop_isTransitive :: Rel Int -> Bool)
 This can also be done by calling the main function.
-}
