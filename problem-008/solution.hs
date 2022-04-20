import System.IO  
import Control.Monad
import Data.Char
import Data.List
import Data.Ord

main = do  
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  let parts = split 13 contents
  -- print parts

  let candidates = map f parts

  let answer = maximum $ map fst candidates -- direct answer

  let answerWithSource = head $ sortDesc candidates 


  print answerWithSource
  print answer

  hClose handle   

split :: Int -> String -> [String]
split n xs
  | length xs < n = []
  | otherwise     = (take 13 xs) : split n (drop 1 xs)


f :: String -> (Int, String)
f digits = (product $ map digitToInt digits, digits)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Down)