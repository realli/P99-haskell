myGcd :: (Integral a) => a -> a -> a
myGcd b a 
	| b ==0 =a
	| otherwise = myGcd (a `mod` b) b
