-- 1
-- 9 7 5 3
-- 25 21 17 13
--

squareAndCorner x = take 4 [sq,sq-x+1..]
  where sq = x^2

diagonals = 1:concatMap squareAndCorner [3,5..1001]

result = sum diagonals
