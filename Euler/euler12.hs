primes = 2 : filter isPrime [3,5..]
isPrime n = null $ tail $ primeFactors n

primeFactors n =
  factors n primes
  where
  factors n (p:ps)
    | p^2 > n = [n]
    | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
    | otherwise = factors n ps


multiplicities [] = []
multiplicities xs = r : multiplicities (drop r xs)
  where
  r = length (takeWhile (== x0) xs)
  (x0:rest) = xs

factorial 0 = 1
factorial n = n * factorial (n-1)

combFactor xs = product $ map factorial (multiplicities xs)

numDivisors n = 2^r `div` combFactor (primeFactors n)
  where r = length (primeFactors n)
