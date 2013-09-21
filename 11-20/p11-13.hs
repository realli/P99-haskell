import Data.List
data Code a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Code a]
encodeModified = map go . group
                where go [x] = Single x
                      go ls@(x:xs) = Multiple (length ls) x

decodeModified :: [Code a] -> [a]
decodeModified = concat . map decodeHelper
                where decodeHelper (Single a) = [a]
                      decodeHelper (Multiple i a) = replicate i a

encode' :: (Eq a, Integral n) => [a] -> [(n,a)]
encode'  = foldr helper [] 
                where helper x [] = [(1, x)]
                      helper x l@(y@(f,s):ys)
                                | x == s = (f+1,s):ys
                                | otherwise = (1,x) : l
encodeDirect :: (Eq a) => [a] -> [Code a]
encodeDirect = map helper . encode'
                where helper (1,x) = Single x
                      helper (n,x) = Multiple n x