module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

{-
 An input-output transition system is a labelled transition system with inputs and outputs ⟨Q, LI , LU , T, q0⟩ where all input actions are enabled in any reachable state.
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

-- Checks if the given IOLTS is valid
validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initial) =
    validateStates states &&
    validateLabels inputs &&
    validateLabels outputs &&
    validateTransitions states transitions &&
    validateInitial states initial &&
    validateTransitionsToExistingStates states transitions &&
    validateTransitionsFromExistingStates states transitions &&
    validateTransitionsWithExistingLabels inputs outputs transitions &&
    validateTransitionsWithExistingStates states transitions

-- Checks if the states in the given IOLTS are unique
validateStates :: [State] -> Bool
validateStates states = length states == length (nub states)

-- Checks if the labels in the given IOLTS are unique
validateLabels :: [Label] -> Bool
validateLabels labels = length labels == length (nub labels)

-- Checks if the transitions in the given IOLTS are unique
validateTransitions :: [State] -> [LabeledTransition] -> Bool
validateTransitions states transitions = length transitions == length (nub transitions)

-- Checks if the initial state in the given IOLTS is in the list of states
validateInitial :: [State] -> State -> Bool
validateInitial states initial = initial `elem` states

-- Checks if the transitions in the given IOLTS are to existing states
validateTransitionsToExistingStates :: [State] -> [LabeledTransition] -> Bool
validateTransitionsToExistingStates states = all (\(_, _, to) -> to `elem` states)

-- Checks if the transitions in the given IOLTS are from existing states
validateTransitionsFromExistingStates :: [State] -> [LabeledTransition] -> Bool
validateTransitionsFromExistingStates states = all (\(from, _, _) -> from `elem` states)

-- Checks if the transitions in the given IOLTS are with existing labels
validateTransitionsWithExistingLabels :: [Label] -> [Label] -> [LabeledTransition] -> Bool
validateTransitionsWithExistingLabels inputs outputs = all (\(_, label, _) -> label `elem` labels)
    where labels = inputs ++ outputs

-- Checks if the transitions in the given IOLTS are with existing states
validateTransitionsWithExistingStates :: [State] -> [LabeledTransition] -> Bool
validateTransitionsWithExistingStates states = all (\(from, _, to) -> from `elem` states && to `elem` states)