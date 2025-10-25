import Data.List (findIndex)
fib = 0:1:(zipWith (+) fib $ tail fib)

solution = findIndex (>=10^999) fib
