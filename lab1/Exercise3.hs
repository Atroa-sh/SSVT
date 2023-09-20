-- Time spent: 2 hours

module Exercise3 where

import Data.List
-- import System.Random
-- import test.QuickCheck
import Lecture2

prop1 :: Int -> Bool
prop1 x = even x && x > 3

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 = even

{-
 Strenght List in descending order
 1. even x && x > 3 (prop1)
 2. even (prop4)
 2. (even x && x > 3) || even x (prop3)
 4. even x || x > 3 (prop2)
 There is a shared second place because those two are equally strong.
-}

-- The strenght list can be printed by calling main or running printPropertyNames sortedProperties.

-- List of all properties which need to be compared to each other to find the strongest.
properties :: [Int -> Bool]
properties = [prop1, prop2, prop3, prop4]

-- Add the propertynames so that they can be printed.
propertyNames :: [String]
propertyNames = ["property1", "property2", "property3", "property4"]

-- Combine properties and propertyNames into pairs so that they can be sorted.
propertiesWithNames :: [(String, Int -> Bool)]
propertiesWithNames = zip propertyNames properties

-- Custom comparison function using the stronger function and Ordering.
compareProperties :: (Int -> Bool) -> (Int -> Bool) -> Ordering
compareProperties x y
    | stronger [-10..10] x y = LT
    | stronger [-10..10] y x = GT
    | otherwise = EQ

-- Sort the properties from strongest to weakest with the sortBy function.
sortedProperties :: [(String, Int -> Bool)]
sortedProperties = sortBy (\(n1, p1) (n2, p2) -> compareProperties p1 p2) propertiesWithNames

-- Print the names of the properties based on the created pairs.
printPropertyNames :: [(String, Int -> Bool)] -> IO ()
printPropertyNames [] = return ()
printPropertyNames ((name, _):rest) = do
    putStrLn name
    printPropertyNames rest

-- Print the names of the properties in descending order of strength.
main :: IO ()
main = do
    putStrLn "The properties in descending order of strength are:"
    printPropertyNames sortedProperties
