{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Euler

test_problem1 = do
    assertEqual 23 $ problem1 10 [3, 5]
    assertEqual 233168 $ problem1 1000 [3, 5]

test_problem2 = do
    assertEqual 4613732 $ problem2 (4 * 10^6)

test_problem3 = do
    assertEqual 29 $ problem3 13195
    assertEqual 6857 $ problem3 600851475143

main :: IO ()
main = htfMain htf_thisModulesTests
