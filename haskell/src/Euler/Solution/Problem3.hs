module Euler.Solution.Problem3 (problem3) where

{-
Largest prime factor
https://projecteuler.net/problem=3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import qualified Data.Map as Map
import Euler.NumberTheory (factorize, primes)

problem3 :: Integer -> Integer
problem3 = last . factorize primes
