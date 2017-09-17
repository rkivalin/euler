module Euler.Solution.Problem19 (problem19) where

{-
Counting Sundays
https://projecteuler.net/problem=19

You are given the following information, but you may prefer to do some research
for yourself.

    - 1 Jan 1900 was a Monday.
    - Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
    - A leap year occurs on any year evenly divisible by 4, but not on a century
      unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?
-}

import Euler.NumberTheory (multipleOf)

data Month = January | February | March | April | May
    | June | July | August | September | October | November | December
    deriving (Eq, Ord, Enum, Bounded, Show)
data Date = Date Int Month Int
    deriving (Eq, Ord, Show)

add days (Date year month date)
    | overflow <= 0 = Date year month nextDate
    | month < maxBound = add overflow $ Date year (succ month) 0
    | otherwise = add overflow $ Date (succ year) minBound 0
    where
        nextDate = date + days
        maxDate = monthLength month year
        overflow = nextDate - maxDate

monthLength :: Month -> Int -> Int
monthLength January _ = 31
monthLength February year
    | leapYear = 29
    | otherwise = 28
    where
        leapYear = year `multipleOf` 4 && not (year `multipleOf` 100) || year `multipleOf` 400
monthLength March _ = 31
monthLength April _ = 30
monthLength May _ = 31
monthLength June _ = 30
monthLength July _ = 31
monthLength August _ = 31
monthLength September _ = 30
monthLength October _ = 31
monthLength November _ = 30
monthLength December _ = 31

problem19 :: Int
problem19 = length . filter firstDay . slice $ sundays where
    slice = dropWhile (<startDate) . takeWhile (<=endDate)
    sundays = iterate (add 7) (Date 1899 December 31)
    firstDay (Date _ _ date) = date == 1
    startDate = Date 1901 January 1
    endDate = Date 2000 December 31
