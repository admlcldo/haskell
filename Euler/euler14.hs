nextCollatz 1 = 1
nextCollatz n = if even n then n `div` 2 else 3*n + 1

lenCollatz a0 = lenIterWhile nextCollatz (/=1) a0

lenIterWhile :: (a -> a) -> (a -> Bool) -> a -> Int
lenIterWhile next notDone start = len start 0 where
    len n m = if notDone n
                then len (next n) (m+1)
                else m
