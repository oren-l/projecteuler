import System.IO
import System.Environment
import Data.List
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let filepath = head args
    contents <- readFile filepath
    printEachLine $ toMatrix contents 

toMatrix :: String -> [[Int]]
toMatrix = map stringToInts . lines 

stringToInts :: String -> [Int]
stringToInts str = map read (words str) :: [Int]

printEachLine :: Show a => [a] -> IO ()
printEachLine = mapM_ print 