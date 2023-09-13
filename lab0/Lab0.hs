
module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

--Ex1
equ1_check :: Int -> Bool
equ1_check n = sum (map (^2) [1..n]) == n*(n + 1)*(2*n + 1)`div` 6

equ2_check :: Int -> Bool
equ2_check n = sum (map (^3) [1..n]) == (n*(n + 1)`div`2)^2


--Ex1
gen :: Gen Int
gen = do
  n <- choose (3, 1000)
  return n
  

test1 = quickCheck $ forAll gen $ equ1_check

test2 = quickCheck $ forAll gen $ equ2_check
-- I'd just gen a bunch of values, fed those functions with them and see if it passes for all, I assume that its exactly what quickcheck does
-- time: about and hour, half of this time was setting up stack env, rest was mostly on finding stupid bug and way to gen positive values


--Ex2




probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- in_range :: Int -> [Float] -> Bool
-- in_range num range = range!!0 < num && range!!1 > num


-- random_test :: [Float] -> Bool

-- random_test nums = do
--   let len = length nums
--   let accepted_bucket_size = [(len/4)*0.95,(len/4)*1.05]
--   let b1 = filter (\x -> x > 0 && x < 0.25) nums
--   let b2 = filter (\x -> x >= 0.25 && x < 0.5) nums
--   let b3 = filter (\x -> x >= 0.5 && x < 0.75) nums
--   let b4 = filter (\x -> x >= 0.75 && x < 1) nums
--   in_range (length b1) accepted_bucket_size && in_range (length b2)
--    accepted_bucket_size && in_range (length b3) accepted_bucket_size &&
--    in_range (length b4) accepted_bucket_size





-- test3 = quickCheck $ probs 10000 $ random_test
-- I gave up after an hour of fixing types mismatches

--ex3

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)



triangle :: Integer -> Integer -> Integer -> Shape

triangle a b c
  | (a == b && b == c && c == a) = Equilateral
  | not (a + b > c && b + c > a && c + a > b) = NoTriangle
  | (a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2) = Rectangular
  | (a == b || b == c || c == a) = Isosceles
  | otherwise = Other

--5 mins 
-- triangle 3 3 3 returns Equilateral
-- triangle 3 4 5 returns Rectangular
-- triangle 4 4 3 returns Isosceles
-- triangle 6 2 3 returns NoTriangle
-- Triangle 7 4 5 returns Other

--ex4


reversibleStream :: [Integer]
reversibleStream = 2 : filter (\x -> (prime x) && (prime $ reversal x)) [3..10000]


reversal :: Integer -> Integer
reversal = read . reverse . show

gen2 :: Gen Integer
gen2 = do
  n <- choose (3, 100000)
  return n

check1 :: Integer -> Bool
check1 n = prime n && prime (reversal n)

check2 n = n == reversal (reversal n)

e4t1 = quickCheck $ forAll gen2 $ check2


rot13 :: [Char] -> [Char]
rot13 = undefined

consecutive101Prime :: Integer
consecutive101Prime = undefined

euler9 :: Integer
euler9 = undefined

euler10 :: Integer
euler10 = undefined

euler49 :: Integer
euler49 = undefined