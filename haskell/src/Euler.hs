module Euler
( problem1
, problem2
, problem3
, problem4
, problem5
) where

import Data.List (foldl', maximum)
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
