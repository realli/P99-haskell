import Data.List (group)
primeFactors n = primeFactors' n 2
  where
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

primeFactorsMult n = map helper . group $ primeFactors n
   where helper ls = (head ls, length ls)


phi m = foldl helper 1 $ primeFactorsMult m
    where helper acc (p,n) = acc * (p-1) * p^(n-1)


totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult m]