-- ghc -funfolding-use-threshold=16 -O2 -optc-O3 solution.hs

import System.Environment

sieve :: Int -> [Int]
sieve n = go [2..(n-1)] []
  where
    go :: [Int] -> [Int] -> [Int]
    go [] res = res
    go (n:ns) res = go [m | m <- ns, m `mod` n /= 0] (n:res)


main = do
  args <- getArgs
  let nStr = head args
  let n = read nStr :: Int
  print $ sum $ sieve n