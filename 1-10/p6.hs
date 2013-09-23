isPalindrome ls = reverse ls == ls


isPalindrome' [] = True
isPalindrome' (x:[]) = True
isPalindrome' ls = head ls == last ls && (isPalindrome'.init.tail $ ls )
