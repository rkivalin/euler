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
solution _ = putStrLn "Solution for this problem is not implemented"
