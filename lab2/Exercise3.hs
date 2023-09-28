module Exercise3 where

import Exercise1
import Exercise2
import Data.List
import Test.QuickCheck
import LTS

-- Generate all the Straces of the given IOLTS
straces :: IOLTS -> [Trace]
straces (states, inputs, outputs, transitions, initial) = undefined