module Exercise2 where

import Exercise1
import Data.List
import Test.QuickCheck
import LTS

-- Generate a random string using QuickCheck
randomString :: Gen String
randomString = listOf1 (elements ['a'..'z'])

-- Generate a random transition using QuickCheck
-- Label sometimes returns a empty string which false the test
-- I do not know how to fix this
transitionGen :: Gen (State, Label, State)
transitionGen = do
    from <- arbitrary `suchThat` (> 0)
    label <- randomString `suchThat` (\x -> x /= tau && x /= delta && x /= "")
    to <- arbitrary `suchThat` (> 0)
    return (from, label, to)

-- Generate a random IOLTS using QuickCheck
ltsGen :: Gen IOLTS
ltsGen = do
    transitions <- listOf1 transitionGen
    return (createIOLTS transitions)

main :: IO ()
main = do
    quickCheck $ forAll ltsGen validateLTS
