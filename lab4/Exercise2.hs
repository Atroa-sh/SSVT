-- Time spent: 1 hour

module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

-- Function to generate the intersection of two Sets
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) _ = Set []
setIntersection _ (Set []) = Set []
setIntersection (Set (x:xs)) set
    | inSet x set = insertSet x (setIntersection (Set xs) set)
    | otherwise = setIntersection (Set xs) set

-- Function to generate the union of two Sets
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) set = set
setUnion set (Set []) = set
setUnion (Set (x:xs)) set
    | inSet x set = setUnion (Set xs) set
    | otherwise = insertSet x (setUnion (Set xs) set)

-- Function to generate the difference of two Sets
-- Source of the definition: https://www.splashlearn.com/math-vocabulary/difference-of-sets#:~:text=The%20difference%20between%20the%20two,set%20B%20from%20set%20A.
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = Set []
setDifference set (Set []) = set
setDifference (Set (x:xs)) set
    | inSet x set = setDifference (Set xs) set
    | otherwise = insertSet x (setDifference (Set xs) set)

-- Test properties
-- Tests for setIntersection: idempotence, commutativity, associativity, distributivity, empty set, subset of inputs
prop_idempotenceIntersect :: (Ord a) => Set a -> Bool
prop_idempotenceIntersect set = setIntersection set set == set

prop_commutativityIntersect :: (Ord a) => Set a -> Set a -> Bool
prop_commutativityIntersect set1 set2 = setIntersection set1 set2 == setIntersection set2 set1

prop_associativityIntersect :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_associativityIntersect set1 set2 set3 = setIntersection (setIntersection set1 set2) set3 == setIntersection set1 (setIntersection set2 set3)

-- We use our self-made setUnion function to test the setIntersection function, assuming that it works correctly.
-- We understand that this could invalidate the property, if the setUnion function does not work correctly.
prop_distributivityIntersect :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_distributivityIntersect set1 set2 set3 = setIntersection set1 (setUnion set2 set3) == setUnion (setIntersection set1 set2) (setIntersection set1 set3)

prop_emptySetIntersect :: (Ord a) => Set a -> Bool
prop_emptySetIntersect set = setIntersection set (Set []) == Set []

-- Test if the intersection of two sets is a subset of both sets
prop_isSubsetOfInputsIntersect :: (Ord a) => Set a -> Set a -> Bool
prop_isSubsetOfInputsIntersect set1 set2 = subSet (setIntersection set1 set2) set1 && subSet (setIntersection set1 set2) set2

-- Tests for Union: idempotence, commutativity, associativity, distributivity, empty set, subset of output
prop_idempotenceUnion :: (Ord a) => Set a -> Bool
prop_idempotenceUnion set = setUnion set set == set

prop_commutativityUnion :: (Ord a) => Set a -> Set a -> Bool
prop_commutativityUnion set1 set2 = setUnion set1 set2 == setUnion set2 set1

prop_associativityUnion :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_associativityUnion set1 set2 set3 = setUnion (setUnion set1 set2) set3 == setUnion set1 (setUnion set2 set3)

-- We use our self-made setIntersection function to test the setUnion function, assuming that it works correctly.
-- We understand that this could invalidate the property, if the setIntersection function does not work correctly.
prop_distributivityUnion :: (Ord a) => Set a -> Set a -> Set a -> Bool
prop_distributivityUnion set1 set2 set3 = setUnion set1 (setIntersection set2 set3) == setIntersection (setUnion set1 set2) (setUnion set1 set3)

prop_emptySetUnion :: (Ord a) => Set a -> Bool
prop_emptySetUnion set = setUnion set (Set []) == set

-- Test if both sets are subsets of the union of the two sets
prop_isSubsetOfOutputUnion :: (Ord a) => Set a -> Set a -> Bool
prop_isSubsetOfOutputUnion set1 set2 = subSet set1 (setUnion set1 set2) && subSet set2 (setUnion set1 set2)

-- Test if the difference of two sets is a subset of the first set
prop_isSubsetOfFirstInput :: (Ord a) => Set a -> Set a -> Bool
prop_isSubsetOfFirstInput set1 set2 = subSet (setDifference set1 set2) set1

-- Main function to run all tests
main :: IO ()
main = do
    putStrLn "Testing setIntersection"
    putStrLn "Using own generaor"
    quickCheck $ forAll randomSet prop_idempotenceIntersect
    quickCheck $ forAll randomSet prop_commutativityIntersect
    quickCheck $ forAll randomSet prop_associativityIntersect
    quickCheck $ forAll randomSet prop_distributivityIntersect
    quickCheck $ forAll randomSet prop_emptySetIntersect
    quickCheck $ forAll randomSet prop_isSubsetOfInputsIntersect
    putStrLn "Using QuickCheck"
    quickCheck (prop_idempotenceIntersect :: Set Int -> Bool)
    quickCheck (prop_commutativityIntersect :: Set Int -> Set Int -> Bool)
    quickCheck (prop_associativityIntersect :: Set Int -> Set Int -> Set Int -> Bool)
    quickCheck (prop_distributivityIntersect :: Set Int -> Set Int -> Set Int -> Bool)
    quickCheck (prop_emptySetIntersect :: Set Int -> Bool)
    quickCheck (prop_isSubsetOfInputsIntersect :: Set Int -> Set Int -> Bool)
    putStrLn "Testing setUnion"
    putStrLn "Using own generator"
    quickCheck $ forAll randomSet prop_idempotenceUnion
    quickCheck $ forAll randomSet prop_commutativityUnion
    quickCheck $ forAll randomSet prop_associativityUnion
    quickCheck $ forAll randomSet prop_distributivityUnion
    quickCheck $ forAll randomSet prop_emptySetUnion
    quickCheck $ forAll randomSet prop_isSubsetOfOutputUnion
    putStrLn "Using QuickCheck"
    quickCheck (prop_idempotenceUnion :: Set Int -> Bool)
    quickCheck (prop_commutativityUnion :: Set Int -> Set Int -> Bool)
    quickCheck (prop_associativityUnion :: Set Int -> Set Int -> Set Int -> Bool)
    quickCheck (prop_distributivityUnion :: Set Int -> Set Int -> Set Int -> Bool)
    quickCheck (prop_emptySetUnion :: Set Int -> Bool)
    quickCheck (prop_isSubsetOfOutputUnion :: Set Int -> Set Int -> Bool)
    putStrLn "Testing setDifference"
    putStrLn "Using own generator"
    quickCheck $ forAll randomSet prop_isSubsetOfFirstInput
    putStrLn "Using QuickCheck"
    quickCheck (prop_isSubsetOfFirstInput :: Set Int -> Set Int -> Bool)

{- Test report
 For both the intersect and union operators we implemented the following mathematical properties:
    - idempotence
    - commutativity
    - associativity
    - distributivity
 We checked whether the intersect with the empty set is the empty set or equal to the input
 for the intersect and union operators, respectively. In addition to this, we checked whether
 the intersect of two sets is a subset of both sets and whether both sets are subsets of the union of the two sets.
 Finally, we checked whether the difference of two sets is a subset of the first set.
 All properties are tested with QuickCheck.
 To test the properties, run main. This will check all properties for the intersect and union operators using quickCheck
 with our own generator from scratch and with the quickCheck generator.
 This can also be done by calling the main function.
-}
