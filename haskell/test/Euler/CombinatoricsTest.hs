{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Euler.CombinatoricsTest (htf_thisModulesTests) where

import Test.Framework
import Euler.Combinatorics
import Euler.Math

test_permutations =
    assertEqual (factorial 5) (length $ permutations 5 [1..5])
