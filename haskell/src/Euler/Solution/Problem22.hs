module Euler.Solution.Problem22 (problem22) where

{-
Names scores
https://projecteuler.net/problem=22

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
containing over five-thousand first names, begin by sorting it into alphabetical order.
Then working out the alphabetical value for each name, multiply this value by
its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which
is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So,
COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

import Data.Char (ord)
import Data.List (sort)
import Euler.Collections (enumerate)

nameScore :: (Int, String) -> Int
nameScore (pos, name) = pos * (sum $ map letterScore name) where
    letterScore a = ord a - ord 'A' + 1

problem22 :: FilePath -> IO Int
problem22 filePath = readFile filePath >>= (return . compute . parse) where
    parse s = read $ "[" ++ s ++ "]" :: [String]
    compute = sum . map nameScore . enumerate . sort
