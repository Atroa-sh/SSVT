module Exercise7 where

import Exercise3
import Exercise5

{-
We can check easily that its not always true with a simple example:

ghci> x = [(1,2),(2,3),(3,4)] 

ghci> trClos (symClos x) gives:
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4)]

ghci> symClos  (trClos x) gives:
[(1,2),(2,1),(1,3),(3,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]

If we call symClos first this will create a situation in which we have symmetrical version of each relation
f.e. [(1,2),(2,1)]. With that transitive relation will create tuples of reflexive relations like [(1,1)].

This mechanism cannot occur if we call trClos first.
Existence of reflexive tuples would only occur if they existed in the very first set.

ghci> x = [(1,2),(2,3),(3,4),(1,1),(2,2),(3,3),(4,4)] 
ghci> symClos  (trClos x)
[(1,1),(1,2),(2,1),(1,3),(3,1),(2,2),(2,3),(3,2),(2,4),(4,2),(3,3),(3,4),(4,3),(4,4)]
ghci> trClos (symClos x) 
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,2),(4,3),(4,4)]

If the initial set contains all reflexive tuples both calls produce the same output.
-}