module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

{-
 An input-output transition system is a labelled transition system with inputs and outputs ⟨Q, LI , LU , T, q0⟩ where all input actions are enabled in any reachable state.
 A labelled transition system with inputs and outputs is a 5-tuple ⟨Q,LI,LU,T,q0⟩ where
    – ⟨Q, LI ∪ LU , T, q0⟩ is a labelled transition system in LTS(LI ∪ LU );
    – LI and LU are countable sets of input labels and output labels, respectively, which are disjoint: LI ∩ LU = ∅.
-}

{-
 List of factors that result in invalid IOLTS's:
    - Duplicate states
    - Duplicate labels
    - Duplicate transitions
    - Initial state not in list of states
    - Transitions to non-existing states
    - Transitions from non-existing states
    - Transitions with non-existing labels
    - Transitions with non-existing states
-}

validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initial) =
    null (inputs `intersect` outputs) &&
    validateTransitions (states, inputs, outputs, transitions, initial)

validateTransitions :: IOLTS -> Bool
validateTransitions (states, inputs, outputs, transitions, initial) =
    createLTS transitions == (states, labels, transitions, initial) where labels = outputs ++ inputs

-- Checks if the states in the given IOLTS are unique
prop_validateStates :: IOLTS -> Bool
prop_validateStates (states, inputs, outputs, transitions, initial) = length states == length (nub states)

-- Checks if the labels in the given IOLTS are unique
prop_validateLabels :: IOLTS -> Bool
prop_validateLabels (states, inputs, outputs, transitions, initial) = length (inputs ++ outputs) == length (nub (inputs ++ outputs))

-- Checks if the transitions in the given IOLTS are unique
prop_validateTransitions :: IOLTS -> Bool
prop_validateTransitions (states, inputs, outputs, transitions, initial) = length transitions == length (nub transitions)

-- Checks if the initial state in the given IOLTS is in the list of states
prop_validateInitial :: IOLTS -> Bool
prop_validateInitial (states, inputs, outputs, transitions, initial) = initial `elem` states

-- Checks if the transitions in the given IOLTS are to existing states
prop_validateTransitionsToExistingStates ::  IOLTS -> Bool
prop_validateTransitionsToExistingStates (states, inputs, outputs, transitions, initial) = all (\(_, _, to) -> to `elem` states) transitions

-- Checks if the transitions in the given IOLTS are from existing states
prop_validateTransitionsFromExistingStates :: IOLTS -> Bool
prop_validateTransitionsFromExistingStates (states, inputs, outputs, transitions, initial) = all (\(from, _, _) -> from `elem` states) transitions

-- Checks if the transitions in the given IOLTS are with existing labels
prop_validateTransitionsWithExistingLabels :: IOLTS -> Bool
prop_validateTransitionsWithExistingLabels (states, inputs, outputs, transitions, initial) = all (\(_, label, _) -> label `elem` labels) transitions
    where labels = inputs ++ outputs

-- Checks if the transitions in the given IOLTS are with existing states
prop_validateTransitionsWithExistingStates :: IOLTS -> Bool
prop_validateTransitionsWithExistingStates (states, inputs, outputs, transitions, intinal) = all (\(from, _, to) -> from `elem` states && to `elem` states) transitions

test_all_props :: IOLTS -> Bool
test_all_props iolts = prop_validateStates iolts &&
                       prop_validateLabels iolts &&
                       prop_validateTransitions iolts &&
                       prop_validateInitial iolts &&
                       prop_validateTransitionsToExistingStates iolts &&
                       prop_validateTransitionsFromExistingStates iolts &&
                       prop_validateTransitionsWithExistingLabels iolts &&
                       prop_validateTransitionsWithExistingStates iolts