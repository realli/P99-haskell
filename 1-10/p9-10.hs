pack ls = foldl go [] ls
          where go [] elem = [[elem]]
                go acc elem = if (head.last) acc == elem
                                then init acc ++ [(elem:last acc)]
                                else acc ++ [[elem]]


encode ls = map go $ pack ls
         where go l = (length l, head l)
