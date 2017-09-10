module Euler
( problem1
, problem2
, problem3
, problem4
, problem5
, problem6
, problem7
, problem8
) where

import Data.List (foldl', maximum, tails)
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

x `multipleOf` y = x `mod` y == 0

x `multipleOfAny` [] = False
x `multipleOfAny` (y:ys) = x `multipleOf` y || x `multipleOfAny` ys

fibonacciSeq :: (Integral a) => [a]
fibonacciSeq = seq (0, 1) where
    seq (a, b) = a : seq (b, a + b)

isPrime :: (Integral a) => a -> Bool
isPrime x = not $ x `multipleOfAny` primes where
    primes = takeWhile (<= isqrt x) primeSeq

primeSeq :: (Integral a) => [a]
primeSeq = 2 : filter isPrime [3,5..]

factorize :: (Integral a) => a -> Map.Map a Int
factorize x = factorize x primeSeq where
    factorize 1 _ = Map.empty
    factorize x (f:fs) = if x `multipleOf` f
        then Map.insertWith (+) f 1 (factorize (x `div` f) (f:fs))
        else factorize x fs

unfactorize :: (Integral a) => Map.Map a Int -> a
unfactorize = product . concatMap replicate' . Map.toList where
    replicate' (factor, count) = replicate count factor

digits :: Integer -> [Int]
digits 0 = [0]
digits x = reverse $ split x where
    split :: Integer -> [Int]
    split 0 = []
    split n = digit : (split rest) where
        digit = fromIntegral $ n `mod` 10
        rest = fromIntegral $ n `div` 10

isPalindrome x = let
    d = digits x
    in d == reverse d

lcm :: (Integral a, Ord a) => [a] -> a
lcm numbers = let
    commonFactors = foldl' (Map.unionWith max) Map.empty $ map factorize numbers
    in unfactorize commonFactors

square :: (Integral a) => a -> a
square x = x * x

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

problem1 :: Integer -> [Integer] -> Integer
problem1 n divisors = foldl' (+) 0 $ filter (`multipleOfAny` divisors) [1..(n-1)]

problem2 :: Integer -> Integer
problem2 n = foldl' (+) 0 $ filter even $ takeWhile (<n) fibonacciSeq

problem3 :: Integer -> Integer
problem3 = fst . Map.findMax . factorize

problem4 :: [Integer] -> Integer
problem4 range = maximum $ filter isPalindrome $ products range range where
    products [] b = []
    products [a] b = map (*a) b
    products (a:as) b = products [a] b ++ (products as b)

problem5 :: [Integer] -> Integer
problem5 = Euler.lcm

problem6 :: [Integer] -> Integer
problem6 range = squareSum - sumOfSquares where
    squareSum = square $ sum range
    sumOfSquares = sum $ map square range

problem7 :: Int -> Integer
problem7 n = primeSeq !! (n - 1)

problem8 :: Int -> Int
problem8 n = maximum $ map product $ windows n number where
    number = map Char.digitToInt "\
        \73167176531330624919225119674426574742355349194934\
        \96983520312774506326239578318016984801869478851843\
        \85861560789112949495459501737958331952853208805511\
        \12540698747158523863050715693290963295227443043557\
        \66896648950445244523161731856403098711121722383113\
        \62229893423380308135336276614282806444486645238749\
        \30358907296290491560440772390713810515859307960866\
        \70172427121883998797908792274921901699720888093776\
        \65727333001053367881220235421809751254540594752243\
        \52584907711670556013604839586446706324415722155397\
        \53697817977846174064955149290862569321978468622482\
        \83972241375657056057490261407972968652414535100474\
        \82166370484403199890008895243450658541227588666881\
        \16427171479924442928230863465674813919123162824586\
        \17866458359124566529476545682848912883142607690042\
        \24219022671055626321111109370544217506941658960408\
        \07198403850962455444362981230987879927244284909188\
        \84580156166097919133875499200524063689912560717606\
        \05886116467109405077541002256983155200055935729725\
        \71636269561882670428252483600823257530420752963450"
