t n = n * (n + 1) `div` 2

p n = n * (3 * n - 1) `div` 2

h n = n * (2 * n - 1)

filterThreeSidedNumbers (t : ts) (p : ps) (h : hs)
  | t == p && p == h = t : filterThreeSidedNumbers ts ps hs
  | t == minimum [t, p, h] = filterThreeSidedNumbers ts (p : ps) (h : hs)
  | p == minimum [t, p, h] = filterThreeSidedNumbers (t : ts) ps (h : hs)
  | h == minimum [t, p, h] = filterThreeSidedNumbers (t : ts) (p : ps) hs

s f = map f [1..]

results = filterThreeSidedNumbers (s t) (s p) (s h)



