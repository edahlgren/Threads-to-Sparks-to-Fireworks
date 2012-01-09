import System.Environment
import Text.Printf
import Data.List

main :: IO ()
main = do
  d <- getArgs
  let end = (map rInt d) !! 0
  let listy = [1..end]
  print $ length listy

rInt :: String -> Int
rInt = read
