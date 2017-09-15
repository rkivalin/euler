module Euler.Solution.Problem10 (problem10) where

{-
Summation of primes
https://projecteuler.net/problem=10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

import Euler.NumberTheory (primes)

problem10 :: Int -> Int
problem10 n = sum $ takeWhile (<n) primes
