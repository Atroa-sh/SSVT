module Exercise3 where

import Exercise1
import Exercise2
import Data.List
import Test.QuickCheck
import LTS

qStates :: IOLTS -> [State]
qStates (states, _, outputs, transitions, _) = filter (`notElem` nonQuiescentStates) states
    where nonQuiescentStates = nub $ map (\(x, _, _) -> x) (filter (\(_,labels,_) -> labels `elem` outputs) transitions)

ourNextTransitions:: [State] -> [LabeledTransition] -> State -> [(State,Label)]
ourNextTransitions quiescentStates lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0] ++ deltas
    where deltas = [(q0, delta) | q0 `elem` quiescentStates]

ourFollowingTransitions:: [State] -> [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
ourFollowingTransitions q lt st ls = [(s:st,ls++[l])| (s,l)<-ourNextTransitions q lt (head st)]

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

-- subList :: [a] -> [a] -> Bool
-- subList (x:xs) (y:ys) =

-- Only works with loopless LTSs, because else the traces is infinitely large.
-- prop_tracesSubOfStraces :: IOLTS -> Bool
-- prop_tracesSubOfStraces (states, inputs, outputs, transitions, initial) = subList (traces $ createLTS transitions) (straces (states, inputs, outputs, transitions, initial))

-- Properties:
-- First item should be empty list, as traces get generated from shortest to longest.
-- Items in the list should get progressively larger (or be of equal size as next trace).
-- Take n traces from the IOLTS, check if they are all in the generated list of straces.
-- If an IOLTS contains no loops, complete traces should be a subset of straces).

-- We thought of making a generator that does not contain any quiescent states and no loops, this way the traces and straces outputs should be equal to each other and both finite.
-- However, we quickly noticed that this was not possible as you can't have any output have an output, but have an LTS with no loops.
-- Because of this every LTS that only contains non-quiescent states should still have equal straces as it has traces, but because both traces and straces are infinite,
-- we are not able to test this.

-- Als traces finite gemaakt kan worden, kan gecheckt worden of straces een superset is als die ook finite is.