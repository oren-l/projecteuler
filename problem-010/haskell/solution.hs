sieve :: Int -> [Int]
sieve n = go [2..(n-1)]
  where
    go :: [Int] -> [Int]
    go [] = []
    go (n:ns) = n : go [m | m <- ns, m `mod` n /= 0]

-- too slow for 2M
answer = sum $ sieve 10
