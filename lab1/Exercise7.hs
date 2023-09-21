import SetOrd
import Test.QuickCheck
import Data.List (sort, intercalate,nub)
import Data.Text.Internal.Fusion.Size (lowerBound)
import Data.List.NonEmpty (NonEmpty(..), toList)

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


instance Show Form where
    show (Prop name) = "Prop " ++ show name
    show (Neg f) = "Neg (" ++ show f ++ ")"
    show (Cnj forms) = "Cnj [" ++ intercalate ", " (map show forms) ++ "]"
    show (Dsj forms) = "Dsj [" ++ intercalate ", " (map show forms) ++ "]"
    show (Impl f1 f2) = "Impl (" ++ show f1 ++ ") (" ++ show f2 ++ ")"
    show (Equiv f1 f2) = "Equiv (" ++ show f1 ++ ") (" ++ show f2 ++ ")"


listOf2 :: Gen a -> Gen [a]
listOf2 gen = sized $ \n ->
  do k <- chooseInt (2,2 `max` n)
     vectorOf k gen

instance Arbitrary Form where
    arbitrary = do
        name <- arbitrary
        -- noEmpty <- getNonEmpty <$> arbitrary 
        frequency
            [ (1, return (Prop name))
            , (2, Neg <$> arbitrary)
            , (2, Cnj <$> (listOf2 arbitrary))
            , (2, Dsj <$> (listOf2 arbitrary))
            , (2, Impl <$> arbitrary <*> arbitrary)
            , (2, Equiv <$> arbitrary <*> arbitrary)
            ]



test = Cnj [Neg (Prop 1), Neg (Prop 1)] 

-- lower bound number of distinct elements
-- upper bound <= numSubFormulas
-- remove duplicates in recursive length
-- recursive count sub removes duplicates

setToList :: Set a -> [a]
setToList (Set xs) = xs

isSubFormOf :: Form -> Form -> Bool
isSubFormOf subForm mainForm =
    subForm `elem` setToList (sub mainForm)

prop_sub_extraction :: Form -> Bool
prop_sub_extraction form =
    let extractedSub = sub form
    in all (`isSubFormOf` form) (setToList extractedSub)

numSubFormulas :: Form -> Int  -- upper bound / count with duplicates
numSubFormulas (Prop _) = 1
numSubFormulas (Neg f) = 1 + numSubFormulas f
numSubFormulas (Cnj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Dsj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Impl f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2
numSubFormulas (Equiv f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2


prop_sub_count :: Form -> Bool --upper bound check
prop_sub_count form =
    let subformulas = setToList (sub form)
        numSub = numSubFormulas form
    in length subformulas <= numSub 


prop_distinct_sub_vs_prop :: Form -> Bool --lower bound check
prop_distinct_sub_vs_prop form =
    let subformulas = setToList (sub form)
        distinctSubformulas = nub subformulas  -- Remove duplicates of subformulas
        propCount = length $ filter isProp subformulas -- filter props from subformulas
    in length distinctSubformulas >= propCount

--check for props in formula
isProp :: Form -> Bool
isProp (Prop _) = True
isProp _ = False


--2.

sub2 :: Form -> [Form]    --recursive subsets
sub2 (Prop x) = [Prop x]
sub2 (Neg f) = nub $ Neg f : sub2 f -- nub function removes duplicates
sub2 (Cnj forms) = nub $ Cnj forms : concatMap sub2 forms --concatMap flattens the resulting list of lists into single list
sub2 (Dsj forms) = nub $ Dsj forms : concatMap sub2 forms
sub2 (Impl f1 f2) = nub $ Impl f1 f2 : sub2 f1 ++ sub2 f2
sub2 (Equiv f1 f2) = nub $ Equiv f1 f2 : sub2 f1 ++ sub2 f2

nsub :: Form -> Int     --count of recursive subsets
nsub = length . sub2

prop_nsub :: Form -> Bool
prop_nsub form =
    let distinctSubCount = nsub form
        upperBoundCount = numSubFormulas form
        subformulas = setToList (sub form)
        distinctSubformulas = nub subformulas  -- Remove duplicates of subformulas
        lowerBoundCount = length $ filter isProp subformulas -- filter props from subformulas
    in distinctSubCount <= upperBoundCount && distinctSubCount >= lowerBoundCount