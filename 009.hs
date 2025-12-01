import Data.List (find)

(^!) :: (Num a) => a -> Int -> a
(^!) x n = x ^ n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let twopows = iterate (^! 2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
      isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
   in head $ dropWhile (not . isRoot) iters

isSquare n = squareRoot n ^ 2 == n

squares = map (^ 2) [1 ..]

pyTriplets =
  [ (squareRoot a, squareRoot b, squareRoot c)
  | c <- squares,
    b <- takeWhile (< c) squares,
    let a = c - b,
    a < b,
    isSquare a
  ]

sumT (a, b, c) = a + b + c

productT (a, b, c) = a * b * c

result = maybe 0 productT $ find ((== 1000) . sumT) pyTriplets
