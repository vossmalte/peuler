-- gcd 0 x = x
-- gcd x 0 = x
-- gcd x y
--   | x>y = gcd (div x y) y
--   | otherwise = gcd x (div y x)
-- oops, gcd is in prelude

scm x y = x*y `div` gcd x y

  -- the smallest common multiple of...
  -- ...prime numbers is their product
  -- ...'teilerfremde' (i.e. greatest common denominator is 1) numbers is their product
  -- ...else: is their product devided by their gcd
  --
  -- a simple stratege: fold over the inputs and iteratively multiply and divide by gcd
smallestCommonMultiple = foldr scm 1

solution = smallestCommonMultiple [1..20]

