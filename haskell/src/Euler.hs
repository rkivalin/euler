module Euler
( problem1
, problem2
) where

import Data.List (foldl')

x `multipleOf` y = x `mod` y == 0

x `multipleOfAny` [] = False
x `multipleOfAny` (y:ys) = x `multipleOf` y || x `multipleOfAny` ys

fibonacciSeq = seq (0, 1) where
    seq (a, b) = a : seq (b, a + b)

problem1 :: Integer -> [Integer] -> Integer
problem1 n divisors = foldl' (+) 0 $ filter (`multipleOfAny` divisors) [1..(n-1)]

problem2 :: Integer -> Integer
problem2 n = foldl' (+) 0 $ filter even $ takeWhile (<n) fibonacciSeq
