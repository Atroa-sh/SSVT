import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1

{-
    Exercise 1:
    Time spent: 30 minutes

    Factorials are not that hard to implement.
    The properties that were chosen to test for the factorials are the following:
    orderEquivalency: Because factorials are calculated by multiplying a series of numbers,
        the outcome has to be the same, whether you start with 'n' and multiply it by 'n - 1',
        or you start with '1' and multiply it by '2' etc.
    Splitable: The factorial of 'n' should be equivalent to 'n * (n - 1)!'

    These properties hold for all integer values greater than or equal to 0. To be able to check this
    a custom generator is made using the Gen construct of QuickCheck. This generator only generates positive numbers.
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n
    |n <= 0 = error "Factorials of negative numbers are undefined."
    |otherwise = n * factorial (n - 1)

reverseFactorial :: Integer -> Integer
reverseFactorial 0 = 1
reverseFactorial n
    |n <= 0 = error "Factorials of negative numbers are undefined."
    |otherwise = reverseFactorial (n - 1) * n

-- Wheter you define a factorial as: n * (n - 1) ... or ... (n - 1) * n,
-- The outcome shoulde be the same.
orderEquivalency :: Integer -> Bool
orderEquivalency n = factorial n == reverseFactorial n

-- n! must equal n * (n-1)!
splitable :: Integer -> Bool
splitable n = factorial n == n * factorial (n - 1)

-- Custom generator that only generates positive integers
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

-- main :: IO ()
main = do
    quickCheck $ forAll genPositiveIntegers orderEquivalency
    quickCheck $ forAll genPositiveIntegers splitable


