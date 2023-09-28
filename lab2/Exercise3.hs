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