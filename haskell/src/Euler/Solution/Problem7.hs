module Euler.Solution.Problem7 (problem7) where

{-
10001st prime
https://projecteuler.net/problem=7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

import Euler.NumberTheory (primes)

problem7 :: Int -> Integer
problem7 = (primes !!) . pred
