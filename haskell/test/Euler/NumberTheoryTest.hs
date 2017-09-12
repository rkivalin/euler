{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Euler.NumberTheoryTest (htf_thisModulesTests) where

import Test.Framework
import Euler.NumberTheory

prop_factorize x = x >= 0 ==> x == (unfactorize . factorize) x
    where types = x::Integer

test_dicksonTriples = do
    assertEqual True $ all isPythagoreanTriple $ take 100 dicksonTriples where
        isPythagoreanTriple (a, b, c) = a * a + b * b == c * c
