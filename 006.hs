square x=  x*x
sumTheSquares = sum . map square
squareTheSum = square . sum

diff x = squareTheSum x - sumTheSquares x 

solution = diff [1..100]
