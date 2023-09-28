module Exercise4 where

import Exercise1
import Exercise2
import Exercise3
import Data.List
import Test.QuickCheck
import LTS

after :: IOLTS -> Trace -> [State]
after (states, inputs, outputs, transitions, initial) trace = after' (states, inputs, outputs, transitions, initial) trace initial
after' (states, inputs, outputs, transitions, initial) [] current = [current]
after' (states, inputs, outputs, transitions, initial) (current_transition:rest) current =
    concatMap (after' (states, inputs, outputs, transitions, initial) rest) next_states
  where
    next_states = nextStates' transitions current current_transition

nextStates' :: [LabeledTransition]->State-> Label -> [State]
nextStates' lt q0 t =  [s' | (s,l,s')<- lt , s == q0, l == t]

flatten2DList :: [[State]] -> [State]
flatten2DList xs = concat xs
