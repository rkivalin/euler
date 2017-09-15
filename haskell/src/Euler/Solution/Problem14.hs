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

import Data.List (maximumBy, foldl')
import Data.Function (on)
import qualified Data.IntMap.Strict as IntMap
import Euler.NumberTheory (collatzNext)

collatzLen :: IntMap.IntMap Int -> Int -> (Int, IntMap.IntMap Int)
collatzLen map x = maybe compute (flip (,) map) lookup where
    lookup = IntMap.lookup x map
    compute = (len, IntMap.insert x len nextMap) where
        (next, nextMap) = collatzLen map $ collatzNext x
        len = 1 + next

problem14 :: Int -> Int
problem14 n = fst max where
    max = maximumBy (compare `on` snd) items
    items = filter ((<n) . fst) $ IntMap.toList map
    map = foldl' (\x -> snd . collatzLen x) (IntMap.singleton 1 1) [1..n]
