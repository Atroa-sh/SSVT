import Test.QuickCheck

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) =  [x:ps | ps <- subsequences xs] ++ subsequences xs 
--[r | ps <- subsequences xs, r <- [(x:ps),ps]] 
-- map (x:) (subsequences xs) ++ subsequences xs     i tried different approaches for better performance but didnt help that much


prop_subsequences_correct :: [Int] -> Bool
prop_subsequences_correct xs = length (subsequences xs) == 2 ^ length(xs) --after 30 tests it takes too long to compute, because powerset has too much elements
