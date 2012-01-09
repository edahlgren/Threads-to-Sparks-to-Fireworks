import Control.Exception
import System.Environment
import Text.Printf
import Control.Parallel.Strategies hiding (parMap)
import Control.Seq
import Control.DeepSeq
import Data.List


main :: IO ()
main = do
  d <- getArgs
  let (spark:ends) = map rInt d
  let nested2flat = flatten [[1..x] | x <- ends]  
  let listy = sparksize spark nested2flat
  x <- evaluate $ deep $ runEval $ parMap length listy
  print $ sum x


rInt :: String -> Int
rInt = read

flatten [] = []
flatten (x:xs) = x ++ flatten xs

sparksize n [] = []
sparksize n xs = (take n xs) : sparksize n (drop n xs)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)

deep :: NFData a => a -> a
deep a = deepseq a a


        
