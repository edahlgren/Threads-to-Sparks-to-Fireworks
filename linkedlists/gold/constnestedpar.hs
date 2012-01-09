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
  let [split, end] = map rInt d
  let listy = [[1..split] | x <- [1..end]]
  evaluate $ deep $ runEval $ parMap length listy
  return ()


rInt :: String -> Int
rInt = read

-- | sparksize n [] = []
-- | sparksize n xs = (take n xs) : sparksize n (drop n xs)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)

deep :: NFData a => a -> a
deep a = deepseq a a
