{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module Exercise2 where

import Data.List
import Data.ByteString (filter)
import Test.QuickCheck
-- import Lecture1

sameElems :: (Eq a) => [a] -> [a] -> Bool
sameElems xs ys = null (xs \\ ys) && null (ys \\ xs)
{-
It can be tested by calling sort and comparing the arrays however this would require input to be Ord which doesnt fit 
isDerangement definition
-}

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y
    | length x /= length y = False
    | not (sameElems x y) = False
    | x == [] = False
    | otherwise = _isDerangement x y
{-
we can check a bunch of properties before doing most of the work and eliminate a lot of incorrect inputs
thats why the outer function do checks and inner does actual calculation 
-}


_isDerangement :: Eq a => [a] -> [a] -> Bool
_isDerangement (x:xs) (y:ys)
  | x == y = False
  | length xs == 0 && length ys == 0 = True
  | otherwise = _isDerangement xs ys


deran:: Int -> [[Int]]
deran n = Data.List.filter (isDerangement [0..n-1]) (Data.List.permutations [0..n-1])
{-
I found this implementation to be the simplest, it is dependant on out isDerangement function
but even if it wasnt we would have to test it anyways
-}


prop_reverseInput :: Eq a => [a] -> [a] -> Bool
prop_reverseInput a b = isDerangement a b == isDerangement b a

prop_unequal :: Eq a => [a] -> [a] -> Bool
prop_unequal a b = if (length a) /= (length b) then isDerangement a b == False else True

prop_sameElements :: Eq a => [a] -> [a] -> Bool
prop_sameElements a b = if not (sameElems a b) then isDerangement a b == False else True

{-
Two latter properties are also checked at the start of the function. Reflective is fairly obious.
At first I thought that transitive relation will also be kept however its not true for arrays longer than 3
-}

main :: IO ()
main = do
  quickCheck (prop_reverseInput :: [Int] -> [Int] -> Bool)
  quickCheck (prop_sameElements :: [Int] -> [Int] -> Bool)
  quickCheck (prop_unequal :: [Int] -> [Int] -> Bool)



  putStrLn (if prop_reverseInput [0,1,2] [1,0,2] then "Passed_reverse" else "Failed_reverse")
  putStrLn (if prop_unequal [0,1,2,3] [1,0,2] then "Passed_unequal" else "Failed_unequal")
  putStrLn (if prop_unequal [0,1,2] [1,0,2] then "Passed_unequal" else "Failed_unequal")
  putStrLn (if prop_sameElements [0,1,2] [3,4,5] then "Passed_transitive" else "Failed_transitive")
  putStrLn (if prop_sameElements [0,1,2,3] [1,0,3,2] then "Passed_transitive" else "Failed_transitive")

{-
I
-}



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

indication of time spent: 5h unfortunately I found description to be fairly vague which caused me to go back a lot on code that I've written previously
-}