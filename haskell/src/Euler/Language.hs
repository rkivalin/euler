module Euler.Language (
    spellNumber,
) where

spellNumber = (seq !!) where
    seq = below20 ++ from20to99 ++ from100to999 ++ from1000to999999
    below20 = ["zero", "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
        "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    from20to99 = concat [w:[w ++ "-" ++ spellNumber i | i <- [1..9]] | w <- tens]
    tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    from100to999 = [formatHundred i j | i <- [1..9], j <- [0..99]]
    formatHundred i 0 = spellNumber i ++ " hundred"
    formatHundred i j = spellNumber i ++ " hundred and " ++ spellNumber j
    from1000to999999 = [formatThousand i j k | i <- [1..999], j <- [0..9], k <- [0..99]]
    formatThousand i 0 0 = spellNumber i ++ " thousand"
    formatThousand i j 0 = spellNumber i ++ " thousand and " ++ formatHundred j 0
    formatThousand i 0 k = spellNumber i ++ " thousand and " ++ spellNumber k
    formatThousand i j k = spellNumber i ++ " thousand " ++ formatHundred j k
