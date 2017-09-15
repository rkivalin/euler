module Euler.Solution.Problem5 (problem5) where

{-
Smallest multiple
https://projecteuler.net/problem=5

2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all
of the numbers from 1 to 20?
-}

import Euler.NumberTheory (lcm)

problem5 :: [Integer] -> Integer
problem5 = Euler.NumberTheory.lcm
