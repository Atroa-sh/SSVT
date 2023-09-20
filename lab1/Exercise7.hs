import SetOrd
import Test.QuickCheck
import Data.List (sort, intercalate)

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


instance Arbitrary Form where
    arbitrary = do
        name <- arbitrary
        frequency
            [ (1, return (Prop name))
            , (2, Neg <$> arbitrary)
            , (2, Cnj <$> listOf arbitrary)
            , (2, Dsj <$> listOf arbitrary)
            , (2, Impl <$> arbitrary <*> arbitrary)
            , (2, Equiv <$> arbitrary <*> arbitrary)
            ]


test = Cnj [Prop 1, Neg (Prop 1)] 

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

numSubFormulas :: Form -> Int
numSubFormulas (Prop _) = 1
numSubFormulas (Neg f) = 1 + numSubFormulas f
numSubFormulas (Cnj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Dsj forms) = 1 + sum (map numSubFormulas forms)
numSubFormulas (Impl f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2
numSubFormulas (Equiv f1 f2) = 1 + numSubFormulas f1 + numSubFormulas f2
