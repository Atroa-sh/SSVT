module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

-- Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs.
-- Next, use automated random testing to check that your implementation is correct.
-- First use your own generator, next use QuickCheck.
-- (Deliverables: implementations, test properties, short test report, indication of time spent.)
-- Time spent: 1 hour

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

-- Test report
-- *Exercise2> quickCheck (prop_intersectSet :: Set Int -> Set Int -> Bool)
-- +++ OK, passed 100 tests.
-- *Exercise2> quickCheck (prop_unionSet :: Set Int -> Set Int -> Bool)
-- +++ OK, passed 100 tests.
-- *Exercise2> quickCheck (prop_differenceSet :: Set Int -> Set Int -> Bool)
-- +++ OK, passed 100 tests.
