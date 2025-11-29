import Data.Char (intToDigit)
import Data.List (permutations, reverse, sort)

primes :: [Int]
primes = 2 : filter sieve [3 ..]
  where
    sieve x = not $ foldl' (||) False $ map (\p -> 0 == mod x p) $ takeWhile (\p -> p * p <= x) primes

isPrime x = x `elem` takeWhile (<= x) primes

isPandigital x = str == ['1' .. (intToDigit $ length str)]
  where
    str = sort $ show x

-- there are no pandigitalPrimes with 8 or 9 digits because then they would be divisible by 3
-- still very slow
-- 7652413
pandigitalPrimes = filter isPandigital $ takeWhile (< 10 ^ 8) primes

-- maybe faster?
sevenDigitPrimes = (takeWhile (<=7654321) . dropWhile (<1234567)) primes
isSevenDigitPrime = flip elem sevenDigitPrimes

pandigitalPrimes' = filter (isSevenDigitPrime . read) $ map reverse $ permutations ['1' .. '7']

