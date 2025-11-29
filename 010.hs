primes :: [Int]
primes = 2 : filter sieve [3 ..]
  where
    sieve x = not $ foldl' (||) False $ map (\p -> 0 == mod x p) $ takeWhile (\p -> p * p <= x) primes

result = sum $ takeWhile (<2*10^6) primes
