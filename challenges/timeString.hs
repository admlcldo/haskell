time :: Int -> Int -> String
time h m
  | h < 0 || h > 12 || m < 0 || m > 59 = "not valid time"
  | m == 0 = hour ++ " " ++ "o'clock"
  | otherwise = mins ++ minWord ++ toWord ++ hour

  where

  mins = case if m <= 30 then m else 60 - m of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "quarter"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
    20 -> "twenty"
    21 -> "twenty one"
    22 -> "twenty two"
    23 -> "twenty three"
    24 -> "twenty four"
    25 -> "twenty five"
    26 -> "twenty six"
    27 -> "twenty seven"
    28 -> "twenty eight"
    29 -> "twenty nine"
    30 -> "half"

  minWord
    | m == 1  || m == 59 = " minute"
    | m == 15 || m == 30 || m == 45 = ""
    | otherwise = " minutes"

  toWord
    | m <= 30 = " past "
    | otherwise = " to "

  hour = case if m <=30 then h else (h + 1) `mod` 12 of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
