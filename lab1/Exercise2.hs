module Exercise2 where

import Test.QuickCheck

-- Base: When n = 0 (A is an empty set), power set only contains empty set
-- So |P(A)| = 2^0 = 1 which is correct
-- IH: Assume that for some positive number k, 
-- if|A| = k, then |P(A)| = 2^k.
-- IS: we want to prove that if |A|= k+1, then |P(A)| = 2^(k+1).
-- Case 1: a is not in a subset from P(A).
-- The power set of A without a is the same as the power set of a set with k elements, 
-- and by our inductive hypothesis, |P(A - {a})| = 2^k.
-- Case 2: a is in subset from P(A).
-- We can remove a from each subset in P(A). |P(A - {a})|, 
-- and by our inductive hypothesis, |P(A - {a})| = 2^k

--we have considered all possible cases, and in each case, we find that |P(A - {a})| = 2^k. 
--for each case, we have either included a or not in the subsets, so the total number of subsets is twice the number of subsets in P(A - {a}).
--which is |P(A)| = 2 * |P(A - {a})| = 2 * 2^k = 2^(k+1).
--by mathematical induction, we have proved that if |A| = k + 1, then |P(A)| = 2^(k+1).

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) =  [x:ps | ps <- subsequences xs] ++ subsequences xs 
--[r | ps <- subsequences xs, r <- [(x:ps),ps]] 
-- map (x:) (subsequences xs) ++ subsequences xs     
--i tried different approaches for better performance but didnt help that much

prop_powerSet_test :: [Int] -> Bool
prop_powerSet_test xs = length (subsequences xs) == 2 ^ length(xs) 


-- 1. Is the property hard to test? If you find that it is, can you given a reason why?

-- it was hard to test because with quickCheck i couldnt see inputs of tests and
-- after around 30 tests it takes too long to compute, because powerset has too much elements
-- i used verboseCheck to see test inputs and determine the problem of tests

-- 2.Give your thoughts on the following issue: When you perform these tests, what are you
-- testing actually? Are you checking a mathematical fact? Or are you testing whether
-- subsequences satisfies a part of its specification? Or are you testing something else still?

-- we are checking a mathematical fact that the size of the power set of finite set is 2^n where n is the size of set
-- its about confirming mathematical property related to sets and power sets 

--time spent 3 hours
--verboseCheck prop_powerSet_test