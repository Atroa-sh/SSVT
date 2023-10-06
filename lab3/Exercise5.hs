module Exercise5 where

import Exercise1
import Exercise3
import Data.List
import Mutation
import Debug.Trace
import Test.QuickCheck
import MultiplicationTable
import FitSpec



splitList :: [[Bool]] -> Int -> ([[Bool]], [[Bool]])
splitList = splitList' [] []

splitList' :: [[Bool]] -> [[Bool]] -> [[Bool]] -> Int -> ([[Bool]], [[Bool]])
splitList' first_res second_res [] n = (first_res, second_res)
splitList' first_res second_res (x:xs) n = do
    let first_part = take n x
    let second_part = drop n x
    splitList' (first_res ++ [first_part]) (second_res ++ [second_part]) xs n
{-
Helper function. Splits list of lists into to parts, each sublist on point n 
Example
[[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]]
gives as output
([[1,2,3],[6,7,8],[11,12,13]],
[[4,5],[9,10],[14,15]])
-}


firstTrueSecondTrue :: [Bool] -> [Bool] -> Bool
firstTrueSecondTrue [] [] = True
firstTrueSecondTrue (x:xs) (y:ys)
    | x && y == False = False
    | otherwise = firstTrueSecondTrue xs ys
{-
Helper function. If value in first list is True it has to be True in the second too.
If its the case for entirety of the list returns True else False
-}

--returns whether 2 sets of properties are equal, takes 1st list of properties, 2nd list of properties,
-- function, number of mutants we want to compare functions with, input of the function
areSetsEquivalent :: [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer -> IO Bool
areSetsEquivalent props1 props2 fut muts n = do
    let combined = props1 ++ props2
    let l1 = Data.List.length props1
    resComb <- determineSurvivors n combined fut muts
    let (res1, res2) = splitList resComb l1
    return ((map or res1) == (map or res2))


--returns whether the mutation results of 1st set are included in the second one. takes 1st list of properties, 2nd list of properties,
-- function, number of mutants we want to compare functions with, input of the function
firstImpliesSecond :: [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Integer -> IO Bool
firstImpliesSecond props1 props2 fut muts n = do
    let combined = props1 ++ props2
    let l1 = Data.List.length props1
    resComb <- determineSurvivors n combined fut muts
    let (res1, res2) = splitList resComb l1
    return (firstTrueSecondTrue (map or res1) (map or res2))

{-
Both functions have essentially same body, difference is just the final evaluation.
We could add a case in which if both lists of properties are equal (meaning have exactly same properties) we return true to avoid
unnecessary calculations. This however would require us to define Eq operation for prop type which is too much work and not in scope

Note about body of those functions. At the start we combine the prop lists, we generate mutants and see their outcome and than split
lists again. This is done because separate calls of determineSurvivors would generate different mutants. We have to temporarily combine
those arrays to get exactly the same mutants for both of them
-}


{-
Time spent: 3h
-}