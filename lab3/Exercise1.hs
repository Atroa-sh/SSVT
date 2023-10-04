module Exercise1 where

import Test.QuickCheck

{-
 Types of outputs which are not yet covered by mutations in mutation.hs
 1. Shuffle the order of the list
 2. Add elements to random positions in the list
 3. Remove all elements from the list (empty list)
 4. Remove elements from the start of the list
 5. Remove elements from the middle of the list
 6. Remove elements from a random positions in the list
 7. Perform arithmic operations on any or all elements in the list (add, sub, mul, div, mod, neg, reverse, etc.)
-}

-- 3:
removeAllElements :: [Integer] -> Gen [Integer]
removeAllElements xs = return []

-- 4:
removeElementsFromStart :: [Integer] -> Gen [Integer]
removeElementsFromStart xs = choose (1, length xs - 1) >>= \x -> return $ drop x xs

-- 5:
removeElementsFromMiddle :: [Integer] -> Gen [Integer]
removeElementsFromMiddle xs = choose (1, length xs - 1) >>= \x -> return $ take x xs ++ drop (x + 1) xs

-- 6:
removeElementsFromRandomPosition :: [Integer] -> Gen [Integer]
removeElementsFromRandomPosition xs = choose (1, length xs - 1) >>= \x -> return $ take (x - 1) xs ++ drop x xs

-- 7:
performArithmicOperations :: [Integer] -> Gen [Integer]
performArithmicOperations xs = return $ map negate xs
