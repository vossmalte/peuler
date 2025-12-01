isPalindrome n = let str = show n in reverse str == str

palindromes = [p | x <- [999, 998 .. 100], y <- [999, 998 .. x], let p = x * y, isPalindrome p]

result = maximum palindromes
