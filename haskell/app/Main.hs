module Main where

import System.Environment
import Euler

main :: IO ()
main = getArgs >>= solution

solution [] = putStrLn "Please pass the problem number as the first argument"
solution ("1":_) = putStrLn . show $ problem1 1000 [3, 5]
solution ("2":_) = putStrLn . show $ problem2 (4 * 10^6)
solution ("3":_) = putStrLn . show $ problem3 600851475143
solution ("4":_) = putStrLn . show $ problem4 [100..999]
solution ("5":_) = putStrLn . show $ problem5 [1..20]
solution ("6":_) = putStrLn . show $ problem6 [1..100]
solution ("7":_) = putStrLn . show $ problem7 10001
solution ("8":_) = putStrLn . show $ problem8 13
solution ("9":_) = putStrLn . show $ problem9 1000
solution ("10":_) = putStrLn . show $ problem10 (2 * 10^6)
solution ("11":_) = putStrLn . show $ problem11 4
solution ("12":_) = putStrLn . show $ problem12 500
solution ("13":_) = putStrLn . show $ problem13 10
solution ("14":_) = putStrLn . show $ problem14 (10^6)
solution ("15":_) = putStrLn . show $ problem15 20 20
solution ("16":_) = putStrLn . show $ problem16 2 1000
solution ("17":_) = putStrLn . show $ problem17 [1..1000]
solution ("18":_) = putStrLn . show $ problem18 problem18_data
solution ("19":_) = putStrLn . show $ problem19
solution ("20":_) = putStrLn . show $ problem20 100
solution ("21":_) = putStrLn . show $ problem21 10000
solution ("22":_) = problem22 "assets/p022_names.txt" >>= putStrLn . show
solution _ = putStrLn "Solution for this problem is not implemented"
