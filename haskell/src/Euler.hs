module Euler
( problem1
, problem2
, problem3
) where

import Data.List (foldl')

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

primeFactors :: (Integral a) => a -> [a]
primeFactors x = factors x primeSeq where
    factors 1 _ = []
    factors x (f:fs) = if x `multipleOf` f then f:(factors (x `div` f) (f:fs)) else factors x fs

problem1 :: Integer -> [Integer] -> Integer
problem1 n divisors = foldl' (+) 0 $ filter (`multipleOfAny` divisors) [1..(n-1)]

problem2 :: Integer -> Integer
problem2 n = foldl' (+) 0 $ filter even $ takeWhile (<n) fibonacciSeq

problem3 :: Integer -> Integer
problem3 = last . primeFactors
