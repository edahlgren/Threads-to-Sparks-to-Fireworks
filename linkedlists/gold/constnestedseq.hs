import Control.Exception
import System.Environment
import Text.Printf
import Data.List

main :: IO ()
main = do
  d <- getArgs
  let [split, end] = map rInt d
  let listy = [[1..split] | x <- [1..end]]
  mapM_ (evaluate . length) listy

rInt :: String -> Int
rInt = read
