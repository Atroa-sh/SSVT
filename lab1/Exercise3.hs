-- Time spent: 30 minutes

module Exercise3 where

-- import Data.List
-- import System.Random
-- import test.QuickCheck
import Lecture2

prop1 :: Int -> Bool
-- prop1 a = stronger [-a .. a] (\x -> even x && x > 3) even
prop1 x = even x && x > 3

prop2 :: Int -> Bool
-- prop2 a = stronger [-a .. a] (\x -> even x || x > 3) even
prop2 x = even x || x > 3

prop3 :: Int -> Bool
-- prop3 a = stronger [-a .. a] (\x -> (even x && x > 3) || even x) even
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
-- prop4 a = stronger [-a .. a] even (\x -> (even x && x > 3) || even x)
prop4 = even

{-
 Strenght List in descending order
 1. even x && x > 3 (prop1)
 2. even (prop4)
 2. (even x && x > 3) || even x (prop3)
 4. even x || x > 3 (prop2)
 There is a shared second place because those two are equally strong.
-}

strengthList :: IO ()
strengthList = do
    putStrLn "1. even x && x > 3"
    putStrLn "2. even"
    putStrLn "2. (even x && x > 3) || even x"
    putStrLn "4. even x || x > 3"
    putStrLn "There is a shared second place because those two are equally strong."

