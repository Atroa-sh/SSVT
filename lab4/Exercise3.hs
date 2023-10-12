-- Time spent: 15 minutes

module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

-- Suppose we implement binary relations as list of pairs, Haskell type [(a,a)] . Assume the following definition:
type Rel a = [(a,a)]

-- Funciton that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs.
-- E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = sort $ nub $ (x,y):(y,x):symClos xs
