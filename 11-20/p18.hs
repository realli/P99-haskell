slice :: [a] -> Int -> Int -> [a]
slice ls start end = [ x | (i,x) <- zip [1..end] ls , i >= start]