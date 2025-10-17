import Data.List (find)
primes :: [Int]
primes = 2:filter sieve [3..]
  where sieve x = not $ foldl' (||) False $ map (\p -> 0==mod x p) $ takeWhile (\p -> p*p<=x) primes

n = 600851475143

-- lazy solution
largestPrimeFactorBruteForce = maximum $ filter (\p->0==mod n p) $ takeWhile (\p->p*p<=n) primes

-- accumulator solution
largestPrimeFactor x = largestPrimeFactorAcc 1 x
largestPrimeFactorAcc acc 1 = acc
largestPrimeFactorAcc acc x = maybe acc (\p -> largestPrimeFactorAcc p (div x p)) $ find (\p ->0==mod x p) $ takeWhile (\p->p*p<=n) primes


