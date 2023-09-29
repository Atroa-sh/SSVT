{- Time spent: 2 hours -}

module Exercise2 where

import Exercise1
import Data.List
import Test.QuickCheck
import LTS

-- Generate a random string that starts with '?' or '!'
randomLabel :: Gen String
randomLabel = do
    firstChar <- elements "?!"
    size <- choose (5, 20)
    restOfStr <- fmap nub $ vectorOf size $ elements ['a'..'z']
    return (firstChar : restOfStr)

-- Generate a random transition using QuickCheck
transitionGen :: Gen (State, Label, State)
transitionGen = do
    from <- arbitrary `suchThat` (> 0)
    label <- randomLabel `suchThat` (\x -> x /= tau && x /= delta && x /= "")
    to <- arbitrary `suchThat` (> 0)
    return (from, label, to)

-- Generate a random transition without loops using QuickCheck
looplessTransitionGen :: Gen (State, Label, State)
looplessTransitionGen = do
    from <- arbitrary `suchThat` (> 0)
    label <- randomLabel `suchThat` (\x -> x /= tau && x /= delta && x /= "")
    to <- arbitrary `suchThat` (> 0)
    if to <= from then looplessTransitionGen else return (from, label, to)

-- Generate a random IOLTS using QuickCheck
ltsGen :: Gen IOLTS
ltsGen = do
    transitions <- listOf2 transitionGen
    return (createIOLTS transitions)

looplessLtsGen :: Gen IOLTS
looplessLtsGen = do
    transitions <- listOf2 looplessTransitionGen
    return (createIOLTS transitions)

-- Generate a list of at least two elements using QuickCheck
listOf2 :: Gen a -> Gen [a]
listOf2 gen = do
    n <- choose (2, 10)
    vectorOf n gen

main :: IO ()
main = do
    quickCheck $ forAll ltsGen validateLTS
    quickCheck $ forAll ltsGen prop_validateStates
    quickCheck $ forAll ltsGen prop_validateLabels
    quickCheck $ forAll ltsGen prop_validateTransitions
    quickCheck $ forAll ltsGen prop_validateInitial
    quickCheck $ forAll ltsGen prop_validateTransitionsToExistingStates
    quickCheck $ forAll ltsGen prop_validateTransitionsFromExistingStates
    quickCheck $ forAll ltsGen prop_validateTransitionsWithExistingLabels
    quickCheck $ forAll ltsGen prop_validateTransitionsWithExistingStates

