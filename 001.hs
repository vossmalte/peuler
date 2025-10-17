-- Problem 1
isDivisibleByThree x = 0 == mod x 3
isDivisibleByFive x = 0 == mod x 5
isPartOfSum1 x = isDivisibleByThree x || isDivisibleByFive x

solution1 = sum $ filter isPartOfSum1 [1..999]
