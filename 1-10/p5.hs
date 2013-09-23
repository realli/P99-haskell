myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' = foldr go [] 
              where go ele acc = ele:acc
