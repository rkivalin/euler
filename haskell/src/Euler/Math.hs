module Euler.Math
( factorial
, square
, isqrt
) where

factorial :: (Integral a) => a -> a
factorial x = product [2..x]

square :: (Integral a) => a -> a
square x = x * x

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral
