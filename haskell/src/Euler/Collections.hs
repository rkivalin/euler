module Euler.Collections
( skipDuplicates
, removeOnce
, windows
, dropLast
, lastN
) where

import Data.List (tails, foldl')

skipDuplicates :: (Eq a) => [a] -> [a]
skipDuplicates [] = []
skipDuplicates (x:[]) = x:[]
skipDuplicates (x:y:zs)
    | x == y = tail
    | otherwise = x:tail
    where tail = skipDuplicates (y:zs)

removeOnce :: (Eq a) => a -> [a] -> [a]
removeOnce _ [] = []
removeOnce x (y:ys)
    | x == y = ys
    | otherwise = y : removeOnce x ys

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

dropLast :: [a] -> [a]
dropLast [] = []
dropLast (x:[]) = []
dropLast (x:xs) = x:(dropLast xs)

lastN :: Int -> [a] -> [a]
lastN n = foldl' (const . tail) <*> drop n
