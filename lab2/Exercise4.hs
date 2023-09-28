module Exercise4 where

import Exercise1
import Exercise2
import Exercise3
import Data.List
import Test.QuickCheck
import LTS

after :: IOLTS -> Trace -> [State]
after (states, inputs, outputs, transitions, initial) trace = after' (states, inputs, outputs, transitions, initial) trace initial
after' iolts [] current = [current]
after' iolts (current_transition:rest) current =
    concatMap (after' iolts rest) next_states
  where
    (_, _, _, transitions, _) = iolts
    next_states = nextStates' transitions current current_transition
{-
I've moved main body of function to after'. This was done because I envisioned this function as recursion 
For that we need current state and the remaining list of transactions as arguments which doesn't fit the given definition of after.
-}
nextStates' :: [LabeledTransition]->State-> Label -> [State]
nextStates' lt q0 t =  [s' | (s,l,s')<- lt , s == q0, l == t]
{-
takes list of transactions, start state and label, and returns a list of all possible end states we may land in
modification of nextTransitions' from LTS.hs to better suit our needs
-}

flatten2DList :: [[State]] -> [State]
flatten2DList xs = concat xs
{-
needed to concat the results into one array, in case our iolts has a lot of branches for same inputs and outputs
-}

--traces function, just for IOLTS
tracesIOLTS :: IOLTS -> [Trace] -- [[Label]]
tracesIOLTS (q, li, lo, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

--Checks if after reaches a state for all possible traces
prop_tracesNonEmpty :: IOLTS -> Bool
prop_tracesNonEmpty iolts =
    let t = take 100 (tracesIOLTS iolts)
        res = map (after iolts) t
    in all (\x -> length x >= 1) res

--Checks if after reaches a state for all possible straces
prop_stracesNonEmpty :: IOLTS -> Bool
prop_stracesNonEmpty iolts =
    let t = take 100 (straces iolts)
        res = map (after iolts) t
    in all (\x -> length x >= 1) res

main :: IO ()
main = do
    quickCheck $ forAll ltsGen prop_tracesNonEmpty
    quickCheck $ forAll ltsGen prop_stracesNonEmpty



{-
Indication of time spent:
Function: 2h
Tests: 

-}
