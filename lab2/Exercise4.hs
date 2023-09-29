{- Time spent: 4 hours -}

module Exercise4 where

import Exercise1
import Exercise2
import Exercise3
import Data.List
import Test.QuickCheck
import LTS

-- The after function takes an IOLTS and a trace and returns a list of states that can be reached after executing the trace.
-- A helper function after' is used to recursively calculate the states that can be reached after executing the trace.
after :: IOLTS -> Trace -> [State]
after (states, inputs, outputs, transitions, initial) trace = after' (states, inputs, outputs, transitions, initial) trace initial

after' :: IOLTS -> Trace -> State -> [State]
after' iolts [] current = [current]
after' iolts (current_transition:rest) current =
    concatMap (after' iolts rest) next_states
  where
    (_, _, _, transitions, _) = iolts
    next_states = nextStates' transitions current current_transition

-- The nextStates' function takes a list of transitions, a state and a label and returns a list of states that can be reached from the given state by executing the given label.
-- Queiscent states are also included in the list of states that can be reached.
-- These are hard coded as they are a special edge case.
nextStates' :: [LabeledTransition]->State-> Label -> [State]
nextStates' lt q0 "delta" = [q0]
nextStates' lt q0 t =  [s' | (s,l,s')<- lt , s == q0, l == t]

--traces function, just for IOLTS so mostly copy from LTS.hs
tracesIOLTS :: IOLTS -> [Trace] -- [[Label]]
tracesIOLTS (q, li, lo, lt, q0) = nub $ map snd (traces' lt [([q0],[])])


-- To test our function we will generate a sample IOLTS.
-- With the help of traces and straces function we will generate traces for those IOLTS.
-- Our function should return a non 0 long list for every example.

-- Checks if after reaches a state for all possible traces.
prop_tracesNonEmpty :: IOLTS -> Bool
prop_tracesNonEmpty iolts =
    let t = tracesIOLTS iolts
        res = map (after iolts) t
    in not (any null res)

-- Checks if after reaches a state for all possible straces.
-- The amount of straces is limited to 100 to prevent infinite loops.
prop_stracesNonEmpty :: IOLTS -> Bool
prop_stracesNonEmpty iolts =
    let t = take 100 (straces iolts)
        res = map (after iolts) t
    in not (any null res)

main :: IO ()
main = do
    quickCheck $ forAll looplessLtsGen prop_tracesNonEmpty
    quickCheck $ forAll ltsGen prop_stracesNonEmpty

{-
 Short test report
 Tests have been done with quickCheck as seen in the main function.
 The first test checks if after reaches a state for all possible traces.
 The second test checks if after reaches a state for all possible straces.
 The amount of straces is limited to 100 to prevent infinite loops.
 The tests have been done with the help of the LTS generators from exercise 2.
 All tests have passed. The second test takes a bit longer to run as it generates more traces.
 -}