divisors n = filter ((0 ==) . mod n) [1 .. (n - 1)]

d = sum . divisors

amicable a = (a /= b) && (a == d b)
  where
    b = d a

result = sum $ filter amicable [1 .. 10000]
