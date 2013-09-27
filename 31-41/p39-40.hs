primesR :: (Integral a) => a -> a -> [a]
primesR min max = filter isPrime [min..max]


isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ [x | x <- [2..s]]
            where s = floor $ sqrt $ fromIntegral n

goldbach :: (Integral a) => a -> [(a,a)]
goldbach n = [(x,y)| x <- primesR 2 n, let y=n-x, isPrime (y)]
        