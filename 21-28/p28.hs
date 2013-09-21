import Data.List
import Data.Function (on)
lsort = sortBy (compare `on` length)


-- 插入排序
lsort' :: [[a]] -> [[a]]
lsort' = foldl insert [] 
			where insert acc ele = filter ((<= length ele). length) acc 
								++ [ele] 
								++ filter ((> length ele). length) acc



lfsort :: [[a]] -> [[a]]
lfsort = concat.lsort.groupBy ((==) `on` length).lsort 