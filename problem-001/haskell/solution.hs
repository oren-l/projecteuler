list :: Int -> [Int]
list n = filter isMul3or5 [1..n-1]
    where
        isMul3or5 x = (x `mod` 3 == 0) || (x `mod` 5 == 0)


sumList :: Int -> Int
sumList n = sum $ list n


test :: Int
test = sumList 10


answer :: Int
answer = sumList 1000
