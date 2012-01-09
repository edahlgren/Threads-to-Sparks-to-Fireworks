import System.Environment
import Text.Printf
import Data.List

main :: IO ()
main = do
  d <- getArgs
  let ends = map rInt d
  let listy = [[1..x] | x <- ends]
  print $ sum $ map length listy

rInt :: String -> Int
rInt = read
