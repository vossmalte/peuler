toReverseBinaryList 0 = []
toReverseBinaryList x = m : toReverseBinaryList d
  where
    (d, m) = divMod x 2

isPalindrome xs = xs == reverse xs

isBinaryAndDecimalPalindrome n = d && b
  where
    d = isPalindrome (show n)
    b = isPalindrome (toReverseBinaryList n)

result = sum $ filter isBinaryAndDecimalPalindrome [1 .. 10 ^ 6]
