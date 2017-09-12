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
, isPalindrome
, Euler.NumberTheory.lcm
, dicksonTriples
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

factorize :: (Integral a) => a -> Map.Map a Int
factorize x = factorize' x primes where
    factorize' 0 _ = Map.singleton 0 1
    factorize' 1 _ = Map.empty
    factorize' x (f:fs) = if x `multipleOf` f
        then Map.insertWith (+) f 1 (factorize' (x `div` f) (f:fs))
        else factorize' x fs

unfactorize :: (Integral a) => Map.Map a Int -> a
unfactorize = product . flatten . Map.toList

flatten :: [(a, Int)] -> [a]
flatten = concatMap replicate' where
    replicate' (e, count) = replicate count e

divisors :: (Integral a) => a -> [a]
divisors = (1:) . map product . allCombinations . flatten . Map.toAscList . factorize

divisorPairs :: (Integral a) => a -> [(a, a)]
divisorPairs x = take len $ zip list $ reverse list where
    list = divisors x
    len = (1 + length list) `div` 2

digits :: (Integral a) => a -> [Int]
digits 0 = [0]
digits x = reverse $ split x where
    split 0 = []
    split n = digit : (split rest) where
        digit = fromIntegral $ n `mod` 10
        rest = fromIntegral $ n `div` 10

isPalindrome :: (Integral a) => a -> Bool
isPalindrome x = let
    d = digits x
    in d == reverse d

lcm :: (Integral a, Ord a) => [a] -> a
lcm numbers =
    let factorizations = map factorize numbers
        commonFactors = foldl' (Map.unionWith max) Map.empty factorizations
    in unfactorize commonFactors

dicksonTriples :: (Integral a) => [(a, a, a)]
dicksonTriples = concatMap triples [2,4..] where
    triples r = map (\(s, t) -> (r + s, r + t, r + s + t)) pairs where
        pairs = map (\d -> (d, sqr `div` d)) $ dropLast $ divisors r where
            sqr = r * r `div` 2
