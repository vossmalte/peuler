primes :: [Int]
primes = 2:filter sieve [3..]
  where sieve x = not $ foldl' (||) False $ map (\p -> 0==mod x p) $ takeWhile (\p -> p*p<=x) primes

prime10001 = primes !! 10000
