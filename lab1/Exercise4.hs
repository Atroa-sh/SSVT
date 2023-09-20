{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module Exercise2 where

import Data.List
import Data.List (permutations)
import Data.ByteString (filter)
import Control.Exception (assert)
-- import Lecture1


isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement x y
    | length x /= length y = False
    | sort x /= sort y = False
    | x == [] = False
    | otherwise = _isDerangement x y


-- we can check a bunch of properties before doing most of the work and eliminate a lot of incorrect inputs
-- thats why the outer function do checks and inner does actual calculation 
_isDerangement :: Eq a => [a] -> [a] -> Bool
_isDerangement (x:xs) (y:ys)
  | x == y = False
  | length xs == 0 && length ys == 0 = True
  | otherwise = _isDerangement xs ys


deran:: Int -> [[Int]]
deran n = Data.List.filter (isDerangement [0..n-1]) (Data.List.permutations [0..n-1])

-- we need to test potentially unwanted inputs, different lengths, empty ones, 
main :: IO ()
main = do
  putStrLn (if isDerangement [0,1,2] [1,0,2] then "Passed1" else "Failed1")
  putStrLn (if isDerangement [0,1,2,3] [1,0,2] then "Passed2" else "Failed2")
  putStrLn (if isDerangement ([]::[Int]) ([]::[Int]) then "Passed3" else "Failed3")
  putStrLn (if isDerangement [0,1,2] [3,4,5] then "Passed4" else "Failed4")
  putStrLn (if isDerangement [0,1,2,3] [1,0,3,2] then "Passed5" else "Failed5")


--order of properties:
-- both inputs need to be lists
-- both lists need to be equal in length
-- both lists need to consists of the same elements
-- no element in those lists can occupy same index

--can we test it automatically?
-- problem is that if we were to automate it we would need a function that would generate derengements and we 
-- would be certain of its corectness. How would we be certain of that? Maybe buy checking if all of its outputs are derangements?
-- that would then require that would correctly check if lists are derangements and cycle repeats