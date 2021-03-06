module Euler.Combinatorics (
    permutations,
    allPermutations,
    combinations,
    allCombinations,
    choose,
    multiChoose,
) where

import Euler.Collections (skipDuplicates, removeOnce)
import Euler.Math ((!\), factorial)

permutations :: (Eq a) => Int -> [a] -> [[a]]
permutations _ [] = []
permutations 1 xs = map (:[]) $ skipDuplicates xs
permutations n xs = concatMap subperm $ skipDuplicates xs where
    subperm el = map (el:) (permutations (n - 1) $ removeOnce el xs)

allPermutations :: (Eq a) => [a] -> [[a]]
allPermutations xs = concatMap (\n -> permutations n xs) [1..length xs]

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (:[]) $ skipDuplicates xs
combinations n xs = concatMap subcomb $ uniqueWithTails xs where
    uniqueWithTails [] = []
    uniqueWithTails (x:xs) = (x, xs):(uniqueWithTails $ dropWhile (==x) xs)
    subcomb (x, xs) = map (x:) $ combinations (n - 1) xs

allCombinations :: (Eq a) => [a] -> [[a]]
allCombinations xs = concatMap (\n -> combinations n xs) [1..length xs]

choose :: Integral a => a -> a -> a
n `choose` k
    | k <= halfN = (n !\ k) `div` (factorial k)
    | otherwise = n `choose` (n - k)
    where
        halfN = n `div` 2

multiChoose :: Integral a => a -> a -> a
n `multiChoose` k = (n + k - 1) `choose` k
