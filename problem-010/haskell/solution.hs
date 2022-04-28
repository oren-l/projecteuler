-- ghc -funfolding-use-threshold=16 -O2 -optc-O3 solution.hs

import System.Environment

sieve :: Int -> [Int]
sieve n = go (n-1) [2..(n-1)] []
  where
    go :: Int -> [Int] -> [Int] -> [Int]
    go _   []     res = res
    go max (n:ns) res
      | n > isqrt max = (n:ns) ++ res
      | otherwise    = go max [m | m <- ns, m `mod` n /= 0] (n:res)

isqrt = floor . sqrt . fromIntegral

main = do
  args <- getArgs
  let nStr = head args
  let n = read nStr :: Int
  print $ sum $ sieve n