{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module Lab1_Kacper where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1


isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement x y
    | length x /= length y = False
    | sort x /= sort y = False
    | otherwise = _isDerangement x y


_isDerangement :: Eq a => [a] -> [a] -> Bool
_isDerangement (x:xs) (y:ys)
  | x == y = False
  | length xs == 0 && length ys == 0 = True
  | otherwise = _isDerangement xs ys


