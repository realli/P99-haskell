elementAt ls 0 = head ls
elementAt ls n = elementAt (tail ls) $ n-1

elementAt' ls n = ls !! (n-1)


