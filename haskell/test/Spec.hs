{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import {-@ HTF_TESTS @-} Euler.MathTest
import {-@ HTF_TESTS @-} Euler.CombinatoricsTest
import {-@ HTF_TESTS @-} Euler.NumberTheoryTest
import {-@ HTF_TESTS @-} EulerTest

main :: IO ()
main = htfMain htf_importedTests
