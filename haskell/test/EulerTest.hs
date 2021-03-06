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

test_problem15 = do
    assertEqual 6 $ problem15 2 2
    assertEqual 137846528820 $ problem15 20 20

test_problem16 = do
    assertEqual 26 $ problem16 2 15
    assertEqual 1366 $ problem16 2 1000

test_problem17 = do
    assertEqual 19 $ problem17 [1..5]
    assertEqual 23 $ problem17 [342]
    assertEqual 20 $ problem17 [115]
    assertEqual 21124 $ problem17 [1..1000]

test_problem18 = do
    assertEqual 23 $ problem18 problem18_test_data
    assertEqual 1074 $ problem18 problem18_data

test_problem19 = do
    assertEqual 171 problem19

test_problem20 = do
    assertEqual 27 $ problem20 10
    assertEqual 648 $ problem20 100

test_problem21 = do
    assertEqual 31626 $ problem21 10000

test_problem22 = do
    problem22 "assets/p022_names.txt" >>= assertEqual 871198282
