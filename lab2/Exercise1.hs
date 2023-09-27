module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

{-
List of factors that result in invalid IOLTS's:
    - Duplicate states
    - Duplicate labels
    - Duplicate transitions
    - Transitions to non-existing states
    - Transitions from non-existing states
    - Transitions with non-existing labels
    - Transitions with non-existing states
-}

-- Checks if the given IOLTS is valid
validateLTS :: IOLTS -> Bool
validateLTS = undefined