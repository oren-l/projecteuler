factors :: [(Int,Int)]
factors = [ (a,b) |  a <- range, b <- range, a >= b]
    where
        range = [99, 98 .. 10]


answer :: Int
answer = head $ filter isPalindrome $ map multiply factors
    where
        isPalindrome n  = (show n) == (reverse.show $ n)
        multiply (a,b)  = a * b
