{- Time Spent: 6 hours 2 persons -}

module Exercise3 where

import Exercise1
import Exercise2
import Data.List
import Test.QuickCheck
import LTS

-- Find all the quiescent states in an IOLTS
qStates :: IOLTS -> [State]
qStates (states, _, outputs, transitions, _) = filter (`notElem` nonQuiescentStates) states
    where nonQuiescentStates = nub $ map (\(x, _, _) -> x) (filter (\(_,labels,_) -> labels `elem` outputs) transitions)

-- Find all the transitions possible from a given state including quiescent states
ourNextTransitions:: [State] -> [LabeledTransition] -> State -> [(State,Label)]
ourNextTransitions quiescentStates lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0] ++ deltas
    where deltas = [(q0, delta) | q0 `elem` quiescentStates]

-- Find all the transitions possible from a given state and add them to the list of pairs
ourFollowingTransitions:: [State] -> [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
ourFollowingTransitions q lt st ls = [(s:st,ls++[l])| (s,l)<-ourNextTransitions q lt (head st)]

-- Helper function to find all the straces
straces':: [LabeledTransition] -> [([State],[Label])] -> [State] -> [([State],[Label])]
straces' lt [] q = []
straces' lt pairs q = pairs ++ straces' lt next q
    where next = concatMap (uncurry $ ourFollowingTransitions q lt) pairs

-- Tau unfiltered implementation
-- straces :: IOLTS -> [Trace]
-- straces (q, li, lu, lt, q0) = nub $ map snd (straces' lt [([q0],[])] (qStates (q, li, lu, lt, q0)))

-- Tau filtered implementation
straces :: IOLTS -> [Trace]
straces (q, li, lu, lt, q0) = nub $ map (filter (/= tau)) (nub $ map snd (straces' lt [([q0],[])] (qStates (q, li, lu, lt, q0))))

-- Generate random traces using IOLTS generator from Exercise2 and straces
randomTraces :: Gen [Trace]
randomTraces = do
    straces <$> ltsGen

-- The first element in the list of straces should be an empty list.
prop_FirstItemEmptyList :: IOLTS -> Bool
prop_FirstItemEmptyList iolts = null (head (straces iolts))

-- The length of the first trace should be smaller or equal to the length of the second trace.
-- We only test the first 20 traces because the list of straces is infinite.
prop_progressivelyLargerTraceLengths :: IOLTS -> Bool
prop_progressivelyLargerTraceLengths iolts =
    all (\(trace1, trace2) -> length trace1 <= length trace2) pairs
  where
    traces = take 20 (straces iolts)  -- Get the first 20 traces
    pairs = zip traces (tail traces)

-- Check if a finite list is an unordered subset of an infinite list
subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList _ [] = False
subList xs (y:ys)
    | null commonElements = subList xs ys
    | otherwise = subList (removeAll commonElements xs) ys
  where
    commonElements = filter (`elem` (y:ys)) xs

-- Helper function to remove all occurrences of elements from a list
removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] ys = ys
removeAll _ [] = []
removeAll (x:xs) ys = removeAll xs (filter (/= x) ys)

-- Only works with loopless LTSs, because else the traces is infinitely large.
prop_tracesSubOfStraces :: IOLTS -> Bool
prop_tracesSubOfStraces (states, inputs, outputs, transitions, initial) = subList (traces $ createLTS transitions) (straces (states, inputs, outputs, transitions, initial))

-- Properties:
-- First item should be empty list, as traces get generated from shortest to longest.
-- Items in the list should get progressively larger (or be of equal size as next trace).
-- If an IOLTS contains no loops, complete traces should be a subset of straces).

-- We thought of making a generator that does not contain any quiescent states and no loops, this way the traces and straces outputs should be equal to each other and both finite.
-- However, we quickly noticed that this was not possible as you can't have any output, and have an LTS with no loops.
-- Because of this every LTS that only contains non-quiescent states should still have equal straces as it has traces, but because both traces and straces are infinite,
-- we are not able to test this.

main :: IO ()
main = do
    quickCheck $ forAll ltsGen prop_FirstItemEmptyList
    quickCheck $ forAll ltsGen prop_progressivelyLargerTraceLengths
    quickCheck $ forAll looplessLtsGen prop_tracesSubOfStraces
