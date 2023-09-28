module Exercise3 where

import Exercise1
import Exercise2
import Data.List
import Test.QuickCheck
import LTS

-- Generate all the Straces of the given IOLTS
straces :: IOLTS -> [Trace]
straces (states, inputs, outputs, transitions, initial) = do
    tracesFromState (states, inputs, outputs, transitions, initial) initial

-- Generate all the traces from the given state
tracesFromState :: IOLTS -> State -> [Trace]
tracesFromState (states, inputs, outputs, transitions, initial) state = do
    let transitionsFromState = filter (\(from, _, _) -> from == state) transitions
    if null transitionsFromState
        then [[]]
        else do
            (from, label, to) <- transitionsFromState
            traces <- tracesFromState (states, inputs, outputs, transitions, initial) to
            return (label : traces)
