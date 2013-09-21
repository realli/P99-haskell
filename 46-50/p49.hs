import Control.Monad (replicateM)
gray :: Int -> [String]
gray n = [ x | x <- replicateM n ['0', '1']]

gray' :: Int -> [String]
gray' 0 = [""]
gray' n = foldr (\s acc ->('0':s):('1':s):acc) [] $ gray' (n-1)