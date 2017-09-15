module Euler.Solution.Problem9 (problem9) where

{-
Special Pythagorean triplet
https://projecteuler.net/problem=9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a² + b² = c²

For example, 3² + 4² = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

import Data.List (find)
import Euler.NumberTheory (dicksonTriples)

problem9 :: Int -> Int
problem9 x = maybe 0 product $ find eq dicksonTriples where
    eq t = sum t == x
    sum (a, b, c) = a + b + c
    product (a, b, c) = a * b * c
