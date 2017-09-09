module Euler 
( problem1
) where

import Data.List (foldl')
        
x `multipleOf` y = x `mod` y == 0

x `multipleOfAny` [] = False
x `multipleOfAny` (y:ys) = x `multipleOf` y || x `multipleOfAny` ys
        
problem1 :: Integer -> [Integer] -> Integer
problem1 n divisors = foldl' (+) 0 $ filter (`multipleOfAny` divisors) [1..(n-1)]
