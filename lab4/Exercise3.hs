module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1
import Exercise2

-- Suppose we implement binary relations as list of pairs, Haskell type [(a,a)] . Assume the
-- following definition:
-- type Rel a = [(a,a)]
-- Use the following declaration:
-- symClos :: Ord a => Rel a -> Rel a
-- to define a function that gives the symmetric closure of a relation, where the relation is
-- represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

-- Time spent: 15 minutes

type Rel a = [(a,a)]

-- Symmetric closure
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = (x,y):(y,x):symClos xs
