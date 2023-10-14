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
prop_symClosIsSymmetric :: Ord a => Rel a -> Bool
prop_symClosIsSymmetric [] = True
prop_symClosIsSymmetric rel = do
    let sym = symClos rel
    all (\(x, y) -> (y, x) `elem` sym) sym

-- Function to check if the result of symClos is the same if it is called twice
prop_symClosIdempotence :: Ord a => Rel a -> Bool
prop_symClosIdempotence [] = True
prop_symClosIdempotence rel = do
    let sym = symClos rel
    sym == symClos sym

-- Function to check that the intersection of a relation and its symmetric closure is itself
prop_symClosIntersection :: Ord a => Rel a -> Bool
prop_symClosIntersection [] = True
prop_symClosIntersection rel = do
    let sym = symClos rel
    let intersection = rel `intersect` sym
    intersection == rel

-- Function to check that the union of a relation and its mirror is the symmetric closure
prop_symClosUnion :: Ord a => Rel a -> Bool
prop_symClosUnion [] = True
prop_symClosUnion rel = do
    let sym = symClos rel
    let uni = rel `union` map (\(x, y) -> (y, x)) rel
    list2set uni == list2set sym

-- Function to check if the original relations are preserved in the symmetric closure
prop_symClosPreservesOriginal :: Ord a => Rel a -> Bool
prop_symClosPreservesOriginal [] = True
prop_symClosPreservesOriginal rel = do
    let sym = symClos rel
    list2set rel `subSet` list2set sym

-- Function to check if a relation is transitive
-- TrClos should always be transitive as it is the transitive closure of a relation
prop_trClosIsTransitive :: Ord a => Rel a -> Bool
prop_trClosIsTransitive [] = True
prop_trClosIsTransitive rel = do
    let tr = trClos rel
    all (\(x, y) -> all (\(a, b) -> y /= a || (x, b) `elem` tr) tr) tr

-- Function to check if the result of trClos is the same if it is called twice
prop_trClosIdempotence :: Ord a => Rel a -> Bool
prop_trClosIdempotence [] = True
prop_trClosIdempotence rel = do
    let tr = trClos rel
    tr == trClos tr

-- Function to check that the intersection of a relation and its transitive closure is itself
prop_trClosIntersection :: Ord a => Rel a -> Bool
prop_trClosIntersection [] = True
prop_trClosIntersection rel = do
    let tr = trClos rel
    let intersection = rel `intersect` tr
    intersection == rel

-- Function to check that the union of the original relation with the transitive closure is the transitive closure
prop_trClosUnion :: Ord a => Rel a -> Bool
prop_trClosUnion [] = True
prop_trClosUnion rel = do
    let tr = trClos rel
    let uni = rel `union` tr
    list2set uni == list2set tr

-- Function to check if the original relations are preserved in the transitive closure
prop_trClosPreservesOriginal :: Ord a => Rel a -> Bool
prop_trClosPreservesOriginal [] = True
prop_trClosPreservesOriginal rel = do
    let tr = trClos rel
    list2set rel `subSet` list2set tr

-- Main function to run all tests
main :: IO ()
main = do
    putStrLn "Testing properties for symClos"
    quickCheck (prop_symClosIsSymmetric :: Rel Int -> Bool)
    quickCheck (prop_symClosIdempotence :: Rel Int -> Bool)
    quickCheck (prop_symClosIntersection :: Rel Int -> Bool)
    quickCheck (prop_symClosPreservesOriginal :: Rel Int -> Bool)
    quickCheck (prop_symClosUnion :: Rel Int -> Bool)

    putStrLn "Testing properties for trClos"
    quickCheck (prop_trClosIsTransitive :: Rel Int -> Bool)
    quickCheck (prop_trClosIdempotence :: Rel Int -> Bool)
    quickCheck (prop_trClosIntersection :: Rel Int -> Bool)
    quickCheck (prop_trClosPreservesOriginal :: Rel Int -> Bool)
    quickCheck (prop_trClosUnion :: Rel Int -> Bool)

{- Test Report:
 For both the symClos and trClos functions we implemented the following mathematical properties:
    - idempotence
    - intersection
    - union
    - preserves original
 We checked whether the result of the function is the same if it is called twice.
 We checked whether the intersection of the original relation and the result of the function is the original relation.
 We checked whether the union of the original relation and the result of the function is the result of the function.
 We checked whether the original relation is a subset of the result of the function.
 We checked whether the result of the function is symmetric/transitive.
 All properties are tested with QuickCheck.
 To test the properties, run main. This will check all properties for the symClos and trClos functions using quickCheck.
 These properties are important because they are mathematical properties that should always hold for the symClos and trClos functions.
 If they do not hold, the functions are not implemented correctly.
 But all the test cases pass, so the functions are implemented correctly.
-}
