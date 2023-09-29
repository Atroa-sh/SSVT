module Exercise5 where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.List
import Data.Set (fromList)
import Debug.Trace
import LTS
import System.IO.Unsafe (unsafePerformIO)

data IOLTS2 = IOLTS2 -- we create own data type for easier manipulation with States and Labels, also our type supports transitions of 4 elements [(State, Label, State, Label)]
  { states :: [State],
    inputs :: [Label],
    outputs :: [Label],
    transitions :: [(State, Label, State, Label)],
    initialState :: State
  }

-- type IOLTS2 = ([State], [Label], [Label], [(State, Label, State, Label)], State)

-- IOLTS for doorImpl1, we fit this implementation of IOLTS2 with door1 configuration so we can use it for testing
doorImpl1IOLTS :: IOLTS2
doorImpl1IOLTS =
  IOLTS2
    { states = [0, 1, 2],
      inputs = ["close", "open", "lock", "unlock"],
      outputs = ["closed", "opened", "locked", "unlocked"],
      transitions =
        [ (0, "close", 1, "closed"),
          (1, "open", 0, "opened"),
          (1, "lock", 2, "locked"),
          (2, "unlock", 1, "unlocked")
        ],
      initialState = 0
    }

{-              first approach to solution without printing where is the problem
prop_testIOLTSwithDoors :: IOLTS2 -> (State -> Label -> (State, Label)) -> Bool
prop_testIOLTSwithDoors iolts door = all checkTransition (transitions iolts)
  where
    checkTransition (state1, input, state2, output) =
      let (newState, newOutput) = door state1 input
       in newState == state2 && newOutput == output
       -}

-- Function to test if the door implementation conforms to the IOLTS with printing values
prop_testIOLTSwithDoorsPrint :: IOLTS2 -> (State -> Label -> (State, Label)) -> IO Bool
prop_testIOLTSwithDoorsPrint iolts door = do
  let mismatches = filter (not . checkTransition) (transitions iolts) -- find all mismatches between IOLTS transitions and door transitions
  if null mismatches
    then do
      putStrLn "All transitions match the IOLTS."
      return True
    else do
      putStrLn "Mismatches between door transitions and IOLTS transitions:"
      printMismatches mismatches
      return False
  where
    checkTransition :: (State, Label, State, Label) -> Bool
    checkTransition (state1, input, state2, output) =
      let (newState, newOutput) = door state1 input -- we take door state and label to get value of (state and label) to compare it to iolts state2 and label2 this way we check both right and left side of doors and iolts
       in (newState, newOutput) == (state2, output) -- check if the generated transition matches any of the valid transitions in the IOLTS

    printMismatches :: [(State, Label, State, Label)] -> IO ()  -- function for printing mismatches to console
    printMismatches [] = return ()
    printMismatches ((state1, input, state2, output) : rest) = do
      let (newState, newOutput) = door state1 input
      putStrLn $
        "For input: " ++ input ++
        ", in state: " ++ show state1 ++
        ", IOLTS expects transition to: " ++ show state2 ++ ", " ++ output ++
        ", but door generates: " ++ show newState ++ ", " ++ newOutput
      printMismatches rest

prop_testIOLTSwithDoors :: IOLTS2 -> (State -> Label -> (State, Label)) -> Bool
prop_testIOLTSwithDoors iolts door = unsafePerformIO $ prop_testIOLTSwithDoorsPrint iolts door


--time spent 8 hours

main :: IO ()
main = do
    putStrLn "Test 1"
    let result1 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl1
    putStrLn $ "Test 1 " ++ if result1 then "passed" else "failed"
    
    putStrLn "Test 2"
    let result2 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl2
    putStrLn $ "Test 2 " ++ if result2 then "passed" else "failed"
    
    putStrLn "Test 3"
    let result3 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl3
    putStrLn $ "Test 3 " ++ if result3 then "passed" else "failed"
    
    putStrLn "Test 4"                                          -- test4 throws exception because door label is not correct which means in our function we cannot find corresponding transition for IOLTS
    let result4 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl2
    putStrLn $ "Test 2 " ++ if result2 then "passed" else "failed"
    

    putStrLn "Test 5"
    let result5 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl5
    putStrLn $ "Test 5 " ++ if result5 then "passed" else "failed"
    
    putStrLn "Test 6"
    let result6 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl6
    putStrLn $ "Test 6 " ++ if result6 then "passed" else "failed"
    
    putStrLn "Test 7"
    let result7 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl7
    putStrLn $ "Test 7 " ++ if result7 then "passed" else "failed"
    
    putStrLn "Test 8"
    let result8 = prop_testIOLTSwithDoors doorImpl1IOLTS doorImpl8
    putStrLn $ "Test 8 " ++ if result8 then "passed" else "failed"

