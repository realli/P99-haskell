import Data.List (genericLength)
coprime :: (Integral a) => a -> a -> Bool
coprime x y =   gcd x y == 1

totient :: (Integral a) => a -> a
totient 1 = 1
totient x = genericLength $ filter (coprime x) [1..x-1]