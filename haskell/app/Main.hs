module Main where

import System.Environment
import Euler

main :: IO ()
main = getArgs >>= solution

solution [] = putStrLn "Please pass the problem number as the first argument"
solution ("1":_) = putStrLn . show $ problem1 1000 [3, 5]
solution _ = putStrLn "Solution for this problem is not implemented"