import Data.Ratio

plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scale (x, y) a = (a * x, a * y)

rotateLeft (x, y) = (-y, x)

normalise (x, y) = (div x g, div y g) where g = gcd x y

inside m (x, y) = (max x y <= m) && (min x y >= 0)

lowerRightCorner m = [(x, y) | x <- [1 .. m], y <- [0 .. x]]

rightAngleAt m p = f [1 ..] ++ f (map ((-1) *) [1 ..])
  where
    f = takeWhile (inside m) . map (plus p . scale (normalise $ rotateLeft p))

-- all triangles where an right angle is in the lower right corner
-- to calculate all triangles we can swap (x,y -> y,x)
-- then we have some duplicates at the (1,1)-curve so we substract those
allLrTriangles m = concatMap (rightAngleAt m) (lowerRightCorner m)

countRectangularTrianglesAtTheDiagonal m = n * n + r * n
  where
    (n, r) = divMod m 2

countTrianglesWithRightAngleAtTheOrigin m = m * m

countAllTriangles m = 2 * length (allLrTriangles m) - 2 * countRectangularTrianglesAtTheDiagonal m + countTrianglesWithRightAngleAtTheOrigin m

solution = countAllTriangles 50
