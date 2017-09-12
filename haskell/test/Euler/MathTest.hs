{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Euler.MathTest (htf_thisModulesTests) where

import Test.Framework
import Euler.Math

prop_isqrt x =
    let root = isqrt x
        lower = square root
        upper = square $ root + 1
        types = x::Integer
    in x >= 0 ==> (lower <= x) && (x < upper)
