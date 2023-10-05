module Exercise2 where

import Exercise1
import Data.List
import Mutation
import Debug.Trace
import Test.QuickCheck
import MultiplicationTable
import FitSpec

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
countSurvivors n = countSurvivors' n 0

countSurvivors' :: Integer -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> IO Integer
countSurvivors' n current _ _ _
    | n <= 0 = return current
countSurvivors' n current props func mutators = do
    rand <- generate (chooseInteger (1, 10))
    let gen = map (\mutator -> mutate' mutator props func rand) mutators
    results <- mapM generate gen
    let survivors = foldl (\i v -> if v then i + 1 else i) 0 (map or results)
    countSurvivors' (n-genericLength mutators) (survivors+current) props func mutators

{-
General note about the function:
The function will not always consider exactly n mutants. From each execution we subtract the amount of mutants we consider.
If the count is above 0, we go with another round of mutants. Because of that if we have 3 mutators, and we give n=10,
we are going to end up with 12 mutants considered as 12 is the closest number above 10 thats devisable by 3. 

We chose recursion as it was easier to organize the parts of the functions that had to be executed this way
Sub function has another argument to track the amount of mutants that survived.

First case for countSurvivors' is a stop for recursion

rand <- generate (chooseInteger (1, 10)): mutate' requires us to give func some input.
To make test more independent we generate those values

let gen = map (\mutator -> mutate' mutator props func rand) mutators
This line allows us to call mutate' for every mutator provided. The values are not calculated just yet.
For this we need another line:  results <- mapM generate gen

At this point our result is a list of lists of Bool ex.
[[False,False,False,False,False],[False,True,False,True,True],[False,False,False,False,False]] 
Each sublist indicates a mutant and each boolean represents whether the mutant was killed (False) or Survived (True) for each property.

If any of those lists returns True - that's a mutant that haven't been killed. That's what next line does
let survivors = foldl (\i v -> if v then i + 1 else i) 0 (map or results)
Our final output is the number of arrays which contain at least one "True"

The number of mutants that survived will differ from one call to another, because of different mutations that can happen.
Here are some examples 
ghci> countSurvivors 4000 MultiplicationTable.multiplicationTableProps MultiplicationTable.multiplicationTable mutators
1578
ghci> countSurvivors 4000 MultiplicationTable.multiplicationTableProps MultiplicationTable.multiplicationTable mutators
1601
ghci> countSurvivors 4000 MultiplicationTable.multiplicationTableProps MultiplicationTable.multiplicationTable mutators
1578

Time spent: around 5h of the entire team's time 💀
-}