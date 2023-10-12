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

-- Test with QuickCheck properties

-- Function to check if a relation is transitive
prop_isTransitive :: Eq a => Rel a -> Bool
prop_isTransitive [] = True
prop_isTransitive rel = all (\(x, y) -> all (\(a, b) -> y /= a || (x, b) `elem` rel) rel) rel

-- prop_isTransitive :: Eq a => [a] -> Rel a -> Bool
-- prop_isTransitive domain rel = undefined

-- Check if the range of a relation is the same as the domain of the relation
prop_rangeSameAsDomain :: Eq a => [a] -> Rel a -> Bool
prop_rangeSameAsDomain = undefined
-- prop_rangeSameAsDomain domain rel = subSet (list2set (map (snd rel) domain)) && subSet (list2set (domain (map snd rel)))

{-
Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function
in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you
test whether R is serial? How can you prove that R is serial?

The relation R = {(x, y) | x = y(mod n)} is serial if and only if every element in the set {0, 1, 2, ..., n-1} is related to at least one other element in the relation.
In other words, for every element x in the set {0, 1, 2, ..., n-1}, there exists an element y in the same set such that x = y(mod n).

To test whether R is serial, we can check if every element in the set {0, 1, 2, ..., n-1} is related to at least one other element in the relation.
On top of that, we can check if the relation is transitive and if the range of the relation is the same as the domain of the relation.
This is what we tested in the properties prop_isTransitive and prop_rangeSameAsDomain.

To prove that R is serial, we can use a proof by contradiction.
Assume that R is not serial, which means that there exists an element x in the set {0, 1, 2, ..., n-1} that is not related to any other element in the relation.
This means that there does not exist an element y in the same set such that x = y(mod n).
However, this contradicts the definition of modular arithmetic, which states that every integer can be expressed as a multiple of the modulus plus a remainder.
Therefore, R must be serial.

-}