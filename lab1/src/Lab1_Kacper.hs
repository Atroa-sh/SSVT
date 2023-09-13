import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y 
    | length x != lenght y = False
    | sort x != sort y = False
    | otherwise = _isDerangement x y 


_isDerangement :: Eq a => [a] -> [a] -> Bool
_isDerangement (x:xs) (y:ys) = if x == y then False else if length xs == 0 && length ys == 0 then True else _isDerangement xs ys


