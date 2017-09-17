module Euler.Solution.Problem15 (problem15) where

{-
Lattice paths
https://projecteuler.net/problem=15

Starting in the top left corner of a 2×2 grid, and only being able
to move to the right and down, there are exactly 6 routes to the bottom right corner.

    https://projecteuler.net/project/images/p015.gif

How many such routes are there through a 20×20 grid?
-}

{-
Each possible path in the grid may be represented using w stars and h bars,
where w is the number of columns, h is number of rows.

    ★ ★ | |
    ★ | ★ |
    ★ | | ★
    | ★ ★ |
    | ★ | ★
    | | ★ ★

Number of combinations is the same as assigning w stars into (h + 1) buckets,
which is the number of ways to choose w buckets from (h + 1) with repetition.
-}

import Euler.Combinatorics (multiChoose)

problem15 :: Integer -> Integer -> Integer
problem15 w h = succ h `multiChoose` w
