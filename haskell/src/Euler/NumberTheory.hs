module Euler.NumberTheory
( primes
, fibs
, multipleOf
, multipleOfAny
, factorize
, unfactorize
, divisors
, divisorPairs
, digits
, undigits
, isPalindrome
, Euler.NumberTheory.lcm
, dicksonTriples
, triangleNumbers
, reverseNumber
) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.List.Ordered as OrderedList
import Euler.Math (isqrt)
import Euler.Collections (dropLast)
import Euler.Combinatorics (allCombinations)

x `multipleOf` y = x `mod` y == 0

x `multipleOfAny` ys = any (x `multipleOf`) ys

isPrime :: (Integral a) => a -> Bool
isPrime x = not $ x `multipleOfAny` checkPrimes where
    checkPrimes = takeWhile (<= isqrt x) primes

primes :: (Integral a) => [a]
primes = 2 : 3 : OrderedList.minus [5,7..] (OrderedList.unionAll [[p*p, p*p+2*p..] | p <- tail primes])

fibs :: (Integral a) => [a]
fibs = seq (0, 1) where
    seq (a, b) = a : seq (b, a + b)

factorize :: (Integral a) => [a] -> a -> Map.Map a Int
factorize _ 0 = Map.singleton 0 1
factorize _ 1 = Map.empty
factorize (f:fs) x
    | x `multipleOf` f = Map.insertWith (+) f 1 (factorize (f:fs) (x `div` f))
    | otherwise = factorize fs x

unfactorize :: (Integral a) => Map.Map a Int -> a
unfactorize = product . flatten . Map.toList

flatten :: [(a, Int)] -> [a]
flatten = concatMap replicate' where
    replicate' (e, count) = replicate count e

divisors :: (Integral a) => [a] -> a -> [a]
divisors primes = (1:) . map product . allCombinations . flatten . Map.toAscList . factorize primes

divisorPairs :: (Integral a) => [a] -> a -> [(a, a)]
divisorPairs primes x = take len $ zip list $ reverse list where
    list = divisors primes x
    len = (1 + length list) `div` 2

digits :: (Integral a) => a -> a -> [a]
digits base x
    | x < base = [x]
    | otherwise = digit : (digits base rest) where
        digit = x `mod` base
        rest = x `div` base

undigits :: (Integral a) => a -> [a] -> a
undigits base = fst . foldl' append (0, 1) where
    append (number, exp) digit = (number + digit * exp, exp * base)

reverseNumber :: (Integral a) => a -> a -> a
reverseNumber base x = rev x 0 where
    rev x prepend
        | x < base = prepend * base + x
        | otherwise = rev rest (prepend * base + lastDigit) where
            lastDigit = mod x base
            rest = div x base

isPalindrome :: (Integral a) => a -> a -> Bool
isPalindrome base x = x == reverseNumber base x

lcm :: (Integral a, Ord a) => [a] -> a
lcm numbers =
    let factorizations = map (factorize primes) numbers
        commonFactors = foldl' (Map.unionWith max) Map.empty factorizations
    in unfactorize commonFactors

dicksonTriples :: (Integral a) => [(a, a, a)]
dicksonTriples = concatMap triples [2,4..] where
    triples r = map (\(s, t) -> (r + s, r + t, r + s + t)) pairs where
        pairs = map (\d -> (d, sqr `div` d)) $ dropLast $ divisors primes r where
            sqr = r * r `div` 2

triangleNumbers :: (Integral a) => [a]
triangleNumbers = seq (1, 2) where
    seq (a, b) = a : seq (a + b, b + 1)
