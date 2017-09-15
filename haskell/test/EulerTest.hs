{-# OPTIONS_GHC -F -pgmF htfpp #-}
module EulerTest (htf_thisModulesTests) where

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

test_problem10 = do
    assertEqual 17 $ problem10 10
    assertEqual 142913828922 $ problem10 (2 * 10^6)

test_problem11 = do
    assertEqual 70600674 $ problem11 4

test_problem12 = do
    assertEqual 28 $ problem12 5
    assertEqual 76576500 $ problem12 500

test_problem13 = do
    assertEqual 5537376230 $ problem13 10

test_problem14 = do
    assertEqual 9 $ problem14 13
    assertEqual 77031 $ problem14 100000
    -- assertEqual 837799 $ problem14 1000000
