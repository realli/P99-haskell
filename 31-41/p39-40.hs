primesR :: (Integral a) => a -> a -> [a]
primesR min max
		| even min = filter isPrime [min+1, min+3..max]
		| otherwise = filter isPrime [min, min+2..max]


isPrime :: (Integral a) => a -> Bool
isPrime n | n <4 = n >1
isPrime n = all ((/= 0). mod n) $ 2:3:[x+i | x <- [6,12..s], i <- [-1,1]]
		where s = floor . sqrt $ fromIntegral n

goldbach :: (Integral a) => a -> [(a,a)]
goldbach n = [(x,y)| x <- primesR 2 n, let y=n-x, isPrime (y)]
		