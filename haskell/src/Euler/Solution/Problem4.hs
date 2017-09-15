module Euler.Solution.Problem4 (problem4) where

{-
Largest palindrome product
https://projecteuler.net/problem=4

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import Data.List (maximum)
import Euler.NumberTheory (isPalindrome)

problem4 :: [Int] -> Int
problem4 range = maximum $ filter (isPalindrome 10) $ products range range where
    products [] b = []
    products [a] b = map (*a) b
    products (a:as) b = products [a] b ++ (products as b)
