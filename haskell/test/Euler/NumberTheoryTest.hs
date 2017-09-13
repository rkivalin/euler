{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Euler.NumberTheoryTest (htf_thisModulesTests) where

import Test.Framework
import Euler.NumberTheory

prop_factorize x = x >= 0 ==> x == (unfactorize . factorize primes) x
    where types = x::Integer

test_dicksonTriples = do
    assertEqual True $ all isPythagoreanTriple $ take 100 dicksonTriples where
        isPythagoreanTriple (a, b, c) = a * a + b * b == c * c

test_triangleNumbers = do
    assertEqual [1, 3, 6, 10, 15, 21, 28] $ take 7 triangleNumbers

prop_reverseNumber x y = x >= 0 && y > 1 && x `mod` y /= 0 ==> x == (reverseNumber y . reverseNumber y) x
    where types = x::Integer

prop_digits x y = x > 0 && y > 1 ==> x == (undigits y . digits y) x
    where types = x::Integer
