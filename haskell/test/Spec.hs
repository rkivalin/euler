{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Euler

test_problem1 = do
    assertEqual 23 $ problem1 10 [3, 5]
    assertEqual 233168 $ problem1 1000 [3, 5]

main :: IO ()
main = htfMain htf_thisModulesTests
