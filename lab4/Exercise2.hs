-- Time spent: 1 hour

module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

-- Set intersection
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) _ = Set []
setIntersection _ (Set []) = Set []
setIntersection (Set (x:xs)) set
    | inSet x set = insertSet x (setIntersection (Set xs) set)
    | otherwise = setIntersection (Set xs) set

-- Set union
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) set = set
setUnion set (Set []) = set
setUnion (Set (x:xs)) set
    | inSet x set = setUnion (Set xs) set
    | otherwise = insertSet x (setUnion (Set xs) set)

-- Set difference
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = Set []
setDifference set (Set []) = set
setDifference (Set (x:xs)) set
    | inSet x set = setDifference (Set xs) set
    | otherwise = insertSet x (setDifference (Set xs) set)

-- Test properties

-- Test if the intersection of two sets is a subset of both sets
prop_intersectSet :: (Ord a) => Set a -> Set a -> Bool
prop_intersectSet set1 set2 = subSet (setIntersection set1 set2) set1 && subSet (setIntersection set1 set2) set2

-- Test if both sets are subsets of the union of the two sets
prop_unionSet :: (Ord a) => Set a -> Set a -> Bool
prop_unionSet set1 set2 = subSet set1 (setUnion set1 set2) && subSet set2 (setUnion set1 set2)

-- Test if the difference of two sets is a subset of the first set
prop_differenceSet :: (Ord a) => Set a -> Set a -> Bool
prop_differenceSet set1 set2 = subSet (setDifference set1 set2) set1

-- Main function to run all tests
main :: IO ()
main = do
    quickCheck (prop_intersectSet :: Set Int -> Set Int -> Bool)
    quickCheck (prop_unionSet :: Set Int -> Set Int -> Bool)
    quickCheck (prop_differenceSet :: Set Int -> Set Int -> Bool)

{- Test report
 For each of the created function one property is tested.
 The first property prop_intersectSet tests the setIntersection function.
 It does this by checking if the intersection of two sets is a subset of both sets.
 This is important because by definition the intersection of two sets should only contain elements that are in both sets.
 The second property prop_unionSet tests the setUnion function.
 It does this by checking if both sets are subsets of the union of the two sets.
 This is important because by definition the union of two sets should contain all elements that are in either of the two sets.
 The third property prop_differenceSet tests the setDifference function.
 It does this by checking if the difference of two sets is a subset of the first set.
 This is important because by definition the difference of two sets should contain all elements that are in the first set but not in the second set.
 All properties are tested with QuickCheck.
 To test the properties, run the following command in ghci:
 ghci> quickCheck (prop_intersectSet :: Set Int -> Set Int -> Bool)
 ghci> quickCheck (prop_unionSet :: Set Int -> Set Int -> Bool)
 ghci> quickCheck (prop_differenceSet :: Set Int -> Set Int -> Bool)
 This can also be done by calling the main function.
-}
