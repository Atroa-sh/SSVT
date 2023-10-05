module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck
import Language.Haskell.TH.Syntax (counter)
-- import Lecture1

{-
    Time: 1 hour
-}
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Helper function that generates all the necessary sublices
subSlices :: [a] -> [[a]]
subSlices [] = []
subSlices (x:xs) = [x] : map (x:) (subSlices xs)

-- Helper function to check if the sum of a list is a prime
prop_sumIsPrime :: [Integer] -> Bool
prop_sumIsPrime n = prime $ product n + 1

-- Function that iterates over all lists of a list and returns all lists
-- that are counterexamples
allCounterExamples :: [[Integer]] -> [[Integer]]
allCounterExamples [] = []
allCounterExamples (x:xs) = if not (prop_sumIsPrime x) then x : allCounterExamples xs else allCounterExamples xs

counterExamples :: [([Integer], Integer)]
counterExamples =
    let slices = subSlices primes
        counterExample = allCounterExamples slices
    in zip counterExample (map (\x -> product x +1) counterExample)
