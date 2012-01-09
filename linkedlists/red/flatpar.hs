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
  let [spark, end] = map rInt d
  let listy = sparksize spark [1..end]
  x <- evaluate $ deep $ runEval $ parMap length listy
  print $ sum x


rInt :: String -> Int
rInt = read

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

