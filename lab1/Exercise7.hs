import SetOrd
import Test.QuickCheck
import Data.List (sort, intercalate,nub)
import Data.Text.Internal.Fusion.Size (lowerBound)
import Data.List.NonEmpty (NonEmpty(..), toList)


-- 1. we can prove that implementation is correct by defining boundaries of minimum and maximum number of subformulas
-- lower bound is number of distinct Props and upper bound is number of all subformulas including duplicates
-- also the function cannot accept empty lists for Cnj and Dsj, however it should accept 2 and more elements in list

-- notes for approach --
-- lower bound number of distinct elements
-- upper bound <= numSubFormulas
-- remove duplicates in recursive length
-- recursive count sub removes duplicates


type Name = Int

data Form = Prop Name
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq,Ord)

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


instance Show Form where        -- used to print to console for better debuging
    show (Prop name) = "Prop " ++ show name
    show (Neg f) = "Neg (" ++ show f ++ ")"
    show (Cnj forms) = "Cnj [" ++ intercalate ", " (map show forms) ++ "]"
    show (Dsj forms) = "Dsj [" ++ intercalate ", " (map show forms) ++ "]"
    show (Impl f1 f2) = "Impl (" ++ show f1 ++ ") (" ++ show f2 ++ ")"
    show (Equiv f1 f2) = "Equiv (" ++ show f1 ++ ") (" ++ show f2 ++ ")"

listOf2 :: Gen a -> Gen [a] --generator that only generates list of 2 elements for Cnj and Dsj for sub function, cannot accept less than 2, also not emplty list
listOf2 gen = do
  n <- getSize
  k <- choose (2,2) --correct implementation of sub should accept more than 2 elements for Cnj and Dsj but not less
  vectorOf k gen    -- for recursive nsub we will create another generator that accepts 2 to n elements

instance Arbitrary Form where
    arbitrary = do
        name <- arbitrary
        -- noEmpty <- getNonEmpty <$> arbitrary
        frequency
            [ (5, return (Prop name))
            , (1, Neg <$> arbitrary)
            , (1, Cnj <$> listOf2 arbitrary)
            , (1, Dsj <$> listOf2 arbitrary)
            , (1, Impl <$> arbitrary <*> arbitrary)
            , (1, Equiv <$> arbitrary <*> arbitrary)
            ]

test = Cnj [Neg (Prop 1), Prop 2]


setToList :: Set a -> [a] --helper function so length can be applied to "set"
setToList (Set xs) = xs

numSubFormulas :: Form -> Int  -- upper bound / count all formulas with duplicates
numSubFormulas (Prop _) = 1
numSubFormulas (Neg f) = 1 + numSubFormulas f
numSubFormulas (Cnj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Dsj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Impl f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2
numSubFormulas (Equiv f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2


prop_upperBoundCheck :: Form -> Bool --this function checks the upper bound of number of formulas
prop_upperBoundCheck form =
    let subformulas = setToList (sub form) --sub form because of removing duplicates, change to list to apply length to variable
        numSub = numSubFormulas form --counts all the formulas including duplicates
    in length subformulas <= numSub --length for number of subformulas, checks if number of formulas is less or equal to upper bound

prop_lowerBoundCheck :: Form -> Bool --lower bound check / checks the number distinct Props in formula, which represents the lower bound when determing number of subformulas
prop_lowerBoundCheck form =
    let subformulas = setToList (sub form) -- same as in upper bound
        distinctSubformulas = nub subformulas  -- Remove duplicates of subformulas / might be redundant
        propCount = length $ filter isProp subformulas -- filter distinct Props from subformulas
    in length distinctSubformulas >= propCount -- lower bound check


--checks if formulas is a prop
isProp :: Form -> Bool
isProp (Prop _) = True
isProp _ = False

prop_inBoundaries :: Form -> Bool  -- upper and lower bundaries check
prop_inBoundaries form =
    let subformulas = setToList (sub form)
        numSub = numSubFormulas form -- upper bound number
        propCount = length $ filter isProp subformulas -- lower bound number
    in length subformulas <= numSub && length subformulas >= propCount -- check if number of formulas is in boundaries

-- time spent 9 hours

--2.

sub2 :: Form -> [Form]    --create all the subformulas of formulas by recursion, similar approach than sub
sub2 (Prop x) = [Prop x]
sub2 (Neg f) = nub $ Neg f : sub2 f -- nub function removes duplicates
sub2 (Cnj forms) = nub $ Cnj forms : concatMap sub2 forms --concatMap flattens the resulting list of lists into single list
sub2 (Dsj forms) = nub $ Dsj forms : concatMap sub2 forms
sub2 (Impl f1 f2) = nub $ Impl f1 f2 : sub2 f1 ++ sub2 f2
sub2 (Equiv f1 f2) = nub $ Equiv f1 f2 : sub2 f1 ++ sub2 f2

nsub :: Form -> Int     --count of recursive subsets
nsub = length . sub2

-- in order to check recursive implementation of nsub we need to create another generator that doesnt test only list of 2 elements for Cnj and Dsj
-- but test 2 to n elements in list, therefore we create another generator, but since we cant specify which generator should quickcheck use we are limited in testing
-- we need to replace old generator with new one, however elements from 2 to n take too long to compute so we try 2 to 5

prop_nsub :: Form -> Bool 
prop_nsub form =          
    let distinctSubCount = nsub form     
        upperBoundCount = numSubFormulas form
        subformulas = sub2 form
        distinctSubformulas = nub subformulas  -- Remove duplicates of subformulas
        lowerBoundCount = length $ filter isProp subformulas -- filter props from subformulas
    in distinctSubCount <= upperBoundCount && distinctSubCount >= lowerBoundCount --checks if number of subformulas is in boundaries


{- listOfn :: Gen a -> Gen [a]
listOfn gen = do
  n <- getSize
  k <- choose (3,5) 
  vectorOf k gen

instance Arbitrary Form where
    arbitrary = do
        name <- arbitrary
        frequency
            [ (5, return (Prop name))
            , (1, Neg <$> arbitrary)
            , (1, Cnj <$> listOf2 arbitrary)
            , (1, Dsj <$> listOf2 arbitrary)
            , (1, Impl <$> arbitrary <*> arbitrary)
            , (1, Equiv <$> arbitrary <*> arbitrary)
            ]
 -}

 -- time spent 6 hours

main = do
    quickCheck $ prop_lowerBoundCheck
    quickCheck $ prop_upperBoundCheck
    quickCheck $ prop_inBoundaries
    quickCheck $ prop_nsub