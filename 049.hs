import Data.List (permutations, groupBy, elem, sortBy, sort)

primes = 2:filter sieve [3..]
  where sieve x = not $ foldl' (||) False $ map (\p -> 0==mod x p) $ takeWhile (\p -> p*p<=x) primes

fourDigitPrimes = (takeWhile (<10000) . dropWhile (<1000)) primes

isPermutationOf a b = elem b (permutations a)

-- at least 3 members of in this permutation group
permutativePrimes = (filter ((>=3) . length) . groupBy isPermutationOf . sortBy (\a b -> compare (sort a) (sort b)) . map show) fourDigitPrimes

haveFixedDistance (a:b:c:xs) = b-a == c-b

solution = (filter (any haveFixedDistance . permutations . map read )) permutativePrimes
