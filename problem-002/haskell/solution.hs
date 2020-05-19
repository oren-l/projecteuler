memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
    where
        fib 0 = 0
        fib 1 = 1
        fib n = memoized_fib (n-2) + memoized_fib (n-1)


fibList :: Integer -> [Integer]
fibList n = takeWhile (<n) $ map memoized_fib [1..]


answer :: Integer
answer = sum $ filter even $ fibList 4000000