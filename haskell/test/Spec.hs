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

test_problem4 = do
    assertEqual 9009 $ problem4 [10..99]
    assertEqual 906609 $ problem4 [100..999]

test_problem5 = do
    assertEqual 2520 $ problem5 [1..10]
    assertEqual 232792560 $ problem5 [1..20]

test_problem6 = do
    assertEqual 2640 $ problem6 [1..10]
    assertEqual 25164150 $ problem6 [1..100]

test_problem7 = do
    assertEqual 13 $ problem7 6
    assertEqual 104743 $ problem7 10001

test_problem8 = do
    assertEqual 5832 $ problem8 4
    assertEqual 23514624000 $ problem8 13

test_problem9 = do
    assertEqual 31875000 $ problem9 1000

main :: IO ()
main = htfMain htf_thisModulesTests
