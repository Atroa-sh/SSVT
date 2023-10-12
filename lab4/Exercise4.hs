module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

-- A relation R is serial on a domain A if for any x ∈ A there is an y ∈ A such that xRy.
-- Suppose relations are represented as lists of pairs:
-- type Rel a = [(a,a)]
-- 1. Write a function for checking whether a relation is serial:
-- isSerial :: Eq a => [a] -> Rel a > Bool
-- For example on the domain [1..5] only [(1, 2), (2, 3), (3, 4), (4, 5), (5, 1)] should be true

type Rel a = [(a,a)]

-- Check if a relation is serial
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial [] _ = True
isSerial (x:xs) rel = elem x (map fst rel) && isSerial xs rel


