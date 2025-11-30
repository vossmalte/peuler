reverseAndAdd n = (read . reverse . show) n + n

isPalindrome n = reverse str == str
  where str = show n

isLychrel = not . any isPalindrome . take 50 . drop 1 .  iterate reverseAndAdd

result = length $ filter isLychrel [1..10000-1]
