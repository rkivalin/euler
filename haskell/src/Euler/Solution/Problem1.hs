module Euler.Solution.Problem1 (problem1) where

{-
Multiples of 3 and 5
https://projecteuler.net/problem=1

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

import Data.List (foldl')
import Euler.NumberTheory (multipleOfAny)

problem1 :: Int -> [Int] -> Int
problem1 n divisors = foldl' (+) 0 $ filter (`multipleOfAny` divisors) [1..(n-1)]
