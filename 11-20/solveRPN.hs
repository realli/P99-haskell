solveRPN ::  String -> Float
solveRPN = head . foldl helper [] . words
            where helper (x:y:xs) "+" = (x+y):xs
                  helper (x:y:xs) "-" = (y-x):xs
                  helper (x:y:xs) "*" = (x*y):xs
                  helper (x:y:xs) "/" = (x/y):xs
                  helper stack c = read c:stack