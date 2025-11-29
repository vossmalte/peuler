import Data.Char (digitToInt)

champernowneDigits = foldMap show [0..]

indices = map (10^) [0..6]

result = product $ map (digitToInt . (champernowneDigits !!)) indices
