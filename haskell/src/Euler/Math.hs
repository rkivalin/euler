module Euler.Math
( factorial
, square
, isqrt
, fallingFactorial
, (!\)
) where

factorial :: (Integral a) => a -> a
factorial x = product [2..x]

fallingFactorial :: Integral a => a -> a -> a
fallingFactorial x n = product [x - i | i <- [0..pred n]]

(!\) :: Integral a => a -> a -> a
(!\) = fallingFactorial

square :: (Integral a) => a -> a
square x = x * x

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral
