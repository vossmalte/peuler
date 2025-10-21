import Data.List (intercalate, singleton)
import Data.Ratio
-- we are talking infinite fractions
-- they are noted in this manner: sqrt 2 = [1; (2)]
sqrt2sequence = 1:cycle [2]
-- that translates to 1 + 1/(2+(1/(2+1/(2+...))))
-- sqrt 23 = [4;(1,3,1,8)]
sqrt23sequence = 4:cycle [1,3,1,8]

-- eulers number can also be written this way
-- e = [2;1,2,1,1,4,1,1,6,1,...]
-- this infinite fraction does not repeat. it's the even numbers padded by ones.
eSequence = 2:1:(intercalate [1,1] $ map singleton [2,4..])

-- let's approximate the values with these infinite fractions
calculateInfiniteFraction ::  (Integral a, Fractional b) => [a] -> b
-- a little dirty because we defince integer sequences above
calculateInfiniteFraction (offset:seq) = fromIntegral offset + foldr (\s acc -> 1/(s+acc)) 0 (map fromIntegral seq)

-- this works really nice but we want to have fractions and 

calculateInfiniteRatio ::  (Integral a) => [a] -> Ratio a
calculateInfiniteRatio (offset:seq) = (offset%1) + foldr (\s acc -> 1/(s+acc)) 0 (map (% 1) seq)

sequenceOfRatios seq = map (\i -> calculateInfiniteRatio $ take i seq ) [1..]

solution = last $ take 100 $ sequenceOfRatios eSequence 
