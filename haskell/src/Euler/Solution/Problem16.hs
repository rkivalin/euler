module Euler.Solution.Problem16 (problem16) where

{-
Power digit sum
https://projecteuler.net/problem=16

2¹⁵ = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2¹⁰⁰⁰?
-}

import Euler.NumberTheory (digits)

problem16 :: Int -> Int -> Int
problem16 base exp = fromIntegral . sum . digits 10 $ number
    where
        number = fromIntegral base^exp :: Integer
