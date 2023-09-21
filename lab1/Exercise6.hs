module Exercise6 where

import Data.List
import Data.Char
import Test.QuickCheck
import Data.Char (GeneralCategory(Format))


update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving (Eq,Ord)

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3 

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concatMap pnames fs
  pnames (Dsj fs) = concatMap pnames fs
  pnames (Impl f1 f2)  = concatMap pnames [f1,f2]
  pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

type Valuation = [(Name,Bool)]

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

type ValFct = Name -> Bool

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain 

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ]
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
nnf (Impl f g) = nnf (Dsj [(Neg f),g])
nnf (Neg (Impl f g)) = nnf (Neg (Dsj [Neg f, g]))
nnf (Equiv f g) = let a = (Cnj [f,g])
                      b = (Cnj [(Neg f), (Neg g)])
                      in nnf (Dsj [a,b])
nnf (Neg (Equiv f g)) = let a = (Dsj [f,g])
                            b = (Dsj [(Neg f), (Neg g)])
                            in nnf (Cnj [a,b])
{-
It wasn't specified in the exercise however since every clause in cnf form is also in nnf form I need nnf working. 
I've implemented missing cases to deal with Implications and Equivalences.
Algorithm follows regular laws of conversion.
-}

cnf :: Form -> Form
cnf f = cnf' (nnf f)
  where
    cnf' :: Form -> Form
    cnf' (Cnj fs) = Cnj (map cnf' fs)
    cnf' (Dsj (x:xs)) = foldr dist (cnf' x) xs
    cnf' (Prop x) = Prop x
    cnf' (Neg (Prop x)) = Neg (Prop x)

    dist :: Form -> Form -> Form
    dist (Cnj fs) f = Cnj (map (dist f) fs)
    dist f (Cnj fs) = Cnj (map (dist f) fs)
    dist f g = Dsj [f, g]

{-
cnf is also a nnf form. We can call nnf first to reduce cnf to only necessary functions.
It doesn't matter if the input is in nnf form already as multiple executionns of nnf will leave same result as just one
dist function stands for distribution law. Foldr allows us to accumulate result of distribution in cases when we have a
disjunction of more than 2 forms.

The algorithm doesn't outputs the simplest possible form. This is for a couple of reasons
1. It was not a requirement
2. Unsymplified form makes it easier to see the work of the algorythm
3. Because of the dalay caused by Exercise4 we were running low on time

Time spent 2,5h
-}