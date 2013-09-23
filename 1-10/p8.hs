compress ls = foldl go [] ls 
            where go acc ele = if isLastElem ele acc 
                                  then acc
                                  else acc ++ [ele]
                                    where isLastElem ele [] = False
                                          isLastElem ele acc = ele == last acc 
