import Data.List (sortOn)


factors :: [(Int,Int)]
factors = [ (a,b) |  a <- range, b <- range, a >= b]
    where
        range = [100 .. 999]


answer :: (Int, (Int,Int))
answer = head $ reverse $ sortOn fst palindromes
    where
        isPalindrome n  = (show n) == (reverse.show $ n)
        palindromes     = [ (res, (a,b)) | (a,b) <- factors, let res = a * b, isPalindrome res ]
