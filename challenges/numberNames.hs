
numName x = let (space:number) = wrap1 x in number

wrap1 x = if x == 0 then " Zero" else wrap2 x

wrap2 x = if x < 0 then " Negative" ++ preNumName (-x) else preNumName x

preNumName n
  | n == 0 = ""
  | n == 1 = " One"
  | n == 2 = " Two"
  | n == 3 = " Three"
  | n == 4 = " Four"
  | n == 5 = " Five"
  | n == 6 = " Six"
  | n == 7 = " Seven"
  | n == 8 = " Eight"
  | n == 9 = " Nine"
  | n == 10 = " Ten"
  | n == 11 = " Eleven"
  | n == 12 = " Twelve"
  | n == 13 = " Thirteen"
  | n == 14 = " Fourteen"
  | n == 15 = " Fifteen"
  | n == 16 = " Sixteen"
  | n == 17 = " Seventeen"
  | n == 18 = " Eighteen"
  | n == 19 = " Nineteen"
  | n < 30 = " Twenty"  ++ preNumName (n `mod` 10)
  | n < 40 = " Thirty" ++ preNumName (n `mod` 10)
  | n < 50 = " Forty" ++ preNumName (n `mod` 10)
  | n < 60 = " Fifty" ++ preNumName (n `mod` 10)
  | n < 70 = " Sixty" ++ preNumName (n `mod` 10)
  | n < 80 = " Seventy" ++ preNumName (n `mod` 10)
  | n < 90 = " Eighty" ++ preNumName (n `mod` 10)
  | n < 100 = " Ninety" ++ preNumName (n `mod` 10)

  | n < 1000 = preNumName (n `div` 100) ++
    let y = n `mod` 100
    in " Hundred" ++ (if y == 0 then "" else " and") ++ preNumName (y)

  | n < 10^6 = preNumName (n `div` 1000) ++ " Thousand" ++ preNumName (n `mod` 1000)
  | n < 10^9 = preNumName (n `div` 10^6) ++ " Million" ++ preNumName (n `mod` 10^6)
  | n < 10^12 = preNumName (n `div` 10^9) ++ " Trillion" ++ preNumName (n `mod` 10^9)
  | n < 10^15 = preNumName (n `div` 10^12) ++ " Quadrillion" ++ preNumName (n `mod` 10^12)
  | n < 10^18 = preNumName (n `div` 10^15) ++ " Quintillion" ++ preNumName (n `mod` 10^15)
  | n < 10^21 = preNumName (n `div` 10^18) ++ " Sextillion" ++ preNumName (n `mod` 10^18)
  | n < 10^24 = preNumName (n `div` 10^21) ++ " Septillion" ++ preNumName (n `mod` 10^21)
  | n < 10^27 = preNumName (n `div` 10^21) ++ " Octillion" ++ preNumName (n `mod` 10^21)
