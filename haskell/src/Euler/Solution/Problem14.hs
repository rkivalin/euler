module Euler.Solution.Problem14 (problem14) where

{-
Longest Collatz sequence
https://projecteuler.net/problem=14

The following iterative sequence is defined for the set of positive integers:

    n → n/2 (n is even)
    n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting
numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Euler.NumberTheory (collatzNext)

collatzLen x map = case HashMap.lookup x map of
    Just len -> (len, map)
    Nothing -> (computed, HashMap.insert x computed nextMap) where
        (next, nextMap) = collatzLen (collatzNext x) map
        computed = 1 + next

problem14 :: Int -> Int
problem14 n = fst max where
    max = maximumBy (compare `on` snd) $ HashMap.toList filteredMap
    filteredMap = HashMap.filterWithKey (\k v -> k < n) map
    map = foldr (\x -> snd . collatzLen x) (HashMap.singleton 1 1) [1..n]
