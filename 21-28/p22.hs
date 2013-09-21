range :: Int -> Int -> [Int]
range a b 
    | a > b = error "error"
    | a == b = [b]
    | otherwise = a : range (a+1) b