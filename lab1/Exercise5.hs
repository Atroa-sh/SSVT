-- Time spent: 1h

module Exercise5 where

import Data.List
import System.Random
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Accusations
-- Matthew: Carl didn't do it, and neither did I.
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true.
accuses :: Boy -> Boy -> Bool
accuses a b
  | a == Matthew = b /= Carl && b /= Matthew
  | a == Peter = b == Matthew || b == Jack
  | a == Jack = not (accuses Matthew b) && not (accuses Peter b)
  | a == Arnold = accuses Matthew b /= accuses Peter b
  | a == Carl = not (accuses Arnold b)
  | otherwise = False

-- Flip the accuses function and filter on the ones they are accused by.
accusers :: Boy -> [Boy]
accusers a = filter (`accuses` a) boys

-- Because we know that 3 boys are telling the truth we can check if the length of the accusers is 3.
-- The three boys in that list are the honest ones.
guilty, honest :: [Boy]
guilty
    | length (accusers Matthew) == 3 = [Matthew]
    | length (accusers Peter) == 3 = [Peter]
    | length (accusers Jack) == 3 = [Jack]
    | length (accusers Arnold) == 3 = [Arnold]
    | length (accusers Carl) == 3 = [Carl]
honest = accusers (head guilty)


{-
 We know that there are 3 boys who are telling the truth. So we need to find a person who is accused by 3 different people.
 Looking at all the possibilities gives us the following cases:
 Matthew, Peter and Jack. Which can not be true because their statements are contradictory.
 Matthew, Peter and Arnold. Which can not be true because their statements are contradictory.
 Matthew, Peter and Carl. This can be true, this leaves us with the culprit Jack.
 Peter, Jack and Arnold. Which can not be true because their statements are contradictory.
 Peter, Jack and Carl. Which can not be true because their statements are contradictory.
 Jack, Arnold and Carl. Which can not be true because their statements are contradictory.
 As we can see there is only one option where a culprit is named and the statements are not contradictory.
 This leaves us with the following solution:
 The guilty one is Jack.
 The honest ones are Matthew, Peter and Carl.
 -}
