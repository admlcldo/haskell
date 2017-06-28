primes = 2 : [n | n <- [3..], and [rem n m > 0 |m <- takeWhile (\x -> x^2 <= n) primes ]] 
