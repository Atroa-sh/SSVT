module Exercise4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1

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
genTuple :: [a] -> Gen (a, a)
genTuple domain = do
    element <- elements domain
    secondElement <- elements domain
    return (element, secondElement)

-- Generate a list of tuples using the genTuple function
genListOfTuples :: [a] -> Gen [(a, a)]
genListOfTuples domain = do
    n <- choose (0, length domain)
    vectorOf n (genTuple domain)

-- Generate a list of tuples which is a serial relation
genSerialRelations :: (Arbitrary a, Ord a) => [a] -> Gen (Rel a)
genSerialRelations domain = do
    rel <- genListOfTuples domain
    if isSerial domain rel then return rel else genSerialRelations domain

-- Check if the range of a relation is the same as the domain of the relation
prop_rangeSameAsDomain :: Ord a => [a] -> Rel a -> Bool
prop_rangeSameAsDomain [] _ = True
prop_rangeSameAsDomain dom rel =
    let range = nub (sort (map fst rel))
    in sort dom == sort range

-- Add property for checking whether the amount of relations in the set is larger or equal to the amount of items in the domain.
prop_lengthOfRelIsLargerOrEqual :: Ord a => [a] -> Rel a -> Bool
prop_lengthOfRelIsLargerOrEqual [] _ = True
prop_lengthOfRelIsLargerOrEqual dom rel = not (isSerial dom rel) || (length rel >= length dom)

main :: IO ()
main = do
    let domain = [0..10] :: [Int]
    quickCheck (forAll (genSerialRelations domain) (prop_rangeSameAsDomain domain))
    quickCheck (forAll (genSerialRelations domain) (prop_lengthOfRelIsLargerOrEqual domain))

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