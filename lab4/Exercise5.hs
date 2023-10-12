module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Exercise1
import Exercise4

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- This function gives the transitive closure of a relation, represented as an ordered list of pairs.
-- E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos rel = do
    let x = sort (nub (rel ++ (rel @@ rel)))
    let len = length x
    if len == length (sort (nub (x ++ (x @@ x)))) then x else trClos x
