import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let filepath = head args
    contents <- readFile filepath
    print $ toMatrix contents 

toMatrix :: String -> [[Int]]
toMatrix = map stringToInts . lines 

stringToInts :: String -> [Int]
stringToInts str = map read (words str) :: [Int]