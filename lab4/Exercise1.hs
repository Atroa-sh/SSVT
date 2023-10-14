-- Time spent: 15 minutes

module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Random generator from scratch
-- We chose a maximum of 20 elements in the set, because otherwise the generator may take too long.
randomSet :: Gen (Set Int)
randomSet = do
    n <- choose (0, 20)
    xs <- randomIntList n
    return (list2set xs)

randomIntList :: Int -> Gen [Int]
randomIntList 0 = return []
randomIntList n = do
    x <- choose (0, n)
    xs <- randomIntList (n-1)
    return (x:xs)

-- Random generator using QuickCheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do
        list2set <$> arbitrary

-- To test the random generator which uses QuickCheck, run the following command in ghci:
-- ghci> sample (arbitrary :: Gen (Set Int))