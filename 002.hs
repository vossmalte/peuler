fib = 1:2:zipWith (+) fib (tail fib)

solution = sum $ filter (\x->0==mod x 2) $ takeWhile (<4000000) fib
