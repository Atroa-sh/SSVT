{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module Exercise2 where

import Data.List
import Data.List (permutations)
import Data.ByteString (filter)
import Test.QuickCheck
-- import Lecture1


isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement x y
    | length x /= length y = False
    | sort x /= sort y = False
    | x == [] = False
    | otherwise = _isDerangement x y
    -- do we need to check if numbers are in 0..n-1 range
    -- are lists with duplicates a valid input


-- we can check a bunch of properties before doing most of the work and eliminate a lot of incorrect inputs
-- thats why the outer function do checks and inner does actual calculation 
_isDerangement :: Eq a => [a] -> [a] -> Bool
_isDerangement (x:xs) (y:ys)
  | x == y = False
  | length xs == 0 && length ys == 0 = True
  | otherwise = _isDerangement xs ys


deran:: Int -> [[Int]]
deran n = Data.List.filter (isDerangement [0..n-1]) (Data.List.permutations [0..n-1])


prop_reverseInput :: Ord a => [a] -> [a] -> Bool
prop_reverseInput a b = isDerangement a b == isDerangement b a

prop_transitive :: Ord a => [a] -> [a] -> [a] -> Bool
prop_transitive a b c = if (a /= c && isDerangement a b == True && isDerangement b c == True) then isDerangement a c == True else True

prop_unequal :: Ord a => [a] -> [a] -> Bool
prop_unequal a b = if (length a) /= (length b) then isDerangement a b == False else True 

prop_sameElements :: Ord a => [a] -> [a] -> Bool
prop_sameElements a b = if (sort a) /= (sort b) then isDerangement a b == False else True

main :: IO ()
main = do
  quickCheck (prop_reverseInput :: [Int] -> [Int] -> Bool)
  quickCheck (prop_sameElements :: [Int] -> [Int] -> Bool)
  quickCheck (prop_unequal :: [Int] -> [Int] -> Bool)
  quickCheck (prop_transitive :: [Int] -> [Int] -> [Int] -> Bool)

  quickCheck $ forAll [[1,2,3],[2,3,1], [], [1,2,3,4]] prop_reverseInput
  -- putStrLn (if isDerangement [0,1,2] [1,0,2] then "Passed1" else "Failed1")
  -- putStrLn (if isDerangement [0,1,2,3] [1,0,2] then "Passed2" else "Failed2")
  -- putStrLn (if isDerangement ([]::[Int]) ([]::[Int]) then "Passed3" else "Failed3")
  -- putStrLn (if isDerangement [0,1,2] [3,4,5] then "Passed4" else "Failed4")
  -- putStrLn (if isDerangement [0,1,2,3] [1,0,3,2] then "Passed5" else "Failed5")



{-
order of properties:
both inputs need to be lists
reflective relation
transitive relation
both lists need to be equal in length  
both lists need to consists of the same elements
no element in those lists can occupy same index
-}

{-
can we test it automatically?
problem is that if we were to automate it we would need a function that would generate derengements and we 
would be certain of its corectness. How would we be certain of that? Maybe buy checking if all of its outputs are derangements?
that would then require that would correctly check if lists are derangements and cycle repeats

another issue is that if we were to tests it would be nice to do it for all possible inputs. 
there is a countable infinity of them so this is not feasale

indication of time spent: 5h
-}