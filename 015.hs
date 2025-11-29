factorial n = product [1..n]

countLatticePaths n = factorial (2*n) `div`  (factorial n)^2

result = countLatticePaths 20
