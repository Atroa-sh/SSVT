module Exercise4 where

import Data.List
import Test.QuickCheck
import LTS

import Data.Set (fromList)


data IOLTS2 = IOLTS2
    { states :: [State]
    , inputs :: [Label]
    , outputs :: [Label]
    , transitions :: [(State, Label, State, Label)]
    , initialState :: State
    }

--type IOLTS2 = ([State], [Label], [Label], [(State, Label, State, Label)], State)

-- IOLTS for doorImpl1
doorImpl1IOLTS :: IOLTS2
doorImpl1IOLTS = IOLTS2
    { states = [0, 1, 2]
    , inputs = ["close", "open", "lock", "unlock"]
    , outputs = ["closed", "opened", "locked", "unlocked"]
    , transitions =
        [ (0, "close", 1, "closed")
        , (1, "open", 0, "opened")
        , (1, "lock", 2, "locked")
        , (2, "unlock", 1, "unlocked")
        ]
    , initialState = 0
    }

-- IOLTS for doorImpl2
doorImpl2IOLTS :: IOLTS2
doorImpl2IOLTS = IOLTS2
    { states = [0, 1, 2]
    , inputs = ["close", "open", "lock", "unlock"]
    , outputs = ["closed", "opened", "locked", "unlocked"]
    , transitions =
        [ (0, "close", 1, "opened")
        , (1, "open", 0, "closed")
        , (1, "lock", 2, "locked")
        , (2, "unlock", 1, "unlocked")
        ]
    , initialState = 0
    }    
  



-- Function to test if the implementation conforms to the IOLTS
testLTSAgainstSUT :: IOLTS2 -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT iolts door = all checkTransition (transitions iolts) --we define 2 IOLTS2 variables
  where
    checkTransition (state1, input, state2, output) =
        let (newState, newOutput) = door state1 input
        in newState == state2 && newOutput == output

      

-- Test doorImpl1 against its IOLTS
main :: IO ()
main = do   ------inputs---------------iolts--------door---
    let result = testLTSAgainstSUT doorImpl1IOLTS doorImpl2 
    if result
        then putStrLn "Implementation conforms to IOLTS."
        else putStrLn "Implementation does not conform to IOLTS."
