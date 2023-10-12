-- Time spent: 15 minutes

module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Implement a random data generator for the datatype Set Int , where Set is as defined in SetOrd.hs.
-- First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
-- (Deliverables: two random test generators, indication of time spent.)

-- Random generator from scratch
randomSet :: IO (Set Int)
randomSet = do
    n <- getRandomInt 10
    xs <- randomIntList n
    return (list2set xs)

randomIntList :: Int -> IO [Int]
randomIntList 0 = return []
randomIntList n = do
    x <- getRandomInt 10
    xs <- randomIntList (n-1)
    return (x:xs)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Random generator using QuickCheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do
        list2set <$> arbitrary

-- To test the random generator which uses QuickCheck, run the following command in ghci:
-- ghci> sample (arbitrary :: Gen (Set Int))