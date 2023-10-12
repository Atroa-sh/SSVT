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

-- Check if a relation is serial and check if elements of the rel are in the domain.
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial [] _ = True
isSerial list rel = usesFullDomain list rel && and (isInDomain list rel)

-- Checks if a relation uses the full domain
-- This needs to be true for a serial relation
usesFullDomain :: Eq a => [a] -> Rel a -> Bool
usesFullDomain [] _ = True
usesFullDomain (x:xs) rel = elem x (map fst rel) && usesFullDomain xs rel

-- Check if all elements in the relation are in a domain
-- This needs to be true for a serial relation
isInDomain :: Eq a => [a] -> Rel a -> [Bool]
isInDomain [] _ = [True]
isInDomain list xs = map (\(a, b) -> a `elem` list && b `elem` list) xs

-- Generate a tuple where the first element comes from the domain list
tupleGenerator :: Arbitrary b => [a] -> Gen (a, b)
tupleGenerator domain = do
    element <- elements domain
    secondElement <- arbitrary
    return (element, secondElement)

-- Generate a list of tuples
listOfTuplesGenerator :: Arbitrary b => [a] -> Gen [(a, b)]
listOfTuplesGenerator domain = do
    n <- choose (0, length domain)
    vectorOf n (tupleGenerator domain)

main :: IO ()
main = do
    let numberOfTuplesToGenerate = 10
    generatedTuples <- generate $ listOfTuplesGenerator [1..5]
    print (generatedTuples :: [(Int,Int)])


rmdups :: (Ord a) => [a] -> [a]
rmdups l = map head (group (sort l))
-- Check if the range of a relation is the same as the domain of the relation
prop_rangeSameAsDomain :: Ord a => [a] -> Rel a -> Bool
prop_rangeSameAsDomain [] _ = True
prop_rangeSameAsDomain dom rel =
    let range = [snd x | x <- rel]
        domainp = sort (rmdups dom)
        rangep = sort (rmdups range)
    in if isSerial dom rel then (domainp == rangep) else True

-- Add property for checking whether the amount of relations in the set is larger or equal to the amount of items in the domain.

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