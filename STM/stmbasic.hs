{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Concurrent.STM
import Control.Monad

-- | A basic Haskell STM function lib
-- | The goal is to make safe, threaded transactions between bank accounts
--    Persons (clients of bank, with accounts) all have: $dollars


newtype Dollars = Dollars Int deriving (Eq, Ord, Show, Num)
type Account = TVar Dollars
data Person = Person {account :: Account}


-- | functions for controlling transactions between Persons' money

-- | 'amount' in Dollars, pulls 'from' account and puts 'to' account
-- | Creates debt (negative Dollars) in 'from' account if insufficient funds
transfer1 :: Dollars -> Account -> Account -> STM ()
transfer1 amount from to = do
  from_balance <- readTVar from
  writeTVar from (from_balance - amount)
  readTVar to >>= writeTVar to . (amount +)
    

-- | Does not allow transfer of funds if 'from' does not have sufficient funds
-- | Mechanism: uses "when x" -> "retry" to wait for 'from' funds to increase
transfer2 :: Dollars -> Account -> Account -> STM ()
transfer2 amount from to = do
  from_balance <- readTVar from
  when (amount > from_balance) $
    retry
  writeTVar from (from_balance - amount)
  readTVar to >>= writeTVar to . (amount +)


-- | Also does not allow transfer when 'from' has insufficient funds
-- | But only tries to take the funds out once, then moves on.  
-- | There is no constant retrying
transfer3 :: Dollars -> Account -> Account -> STM ()
transfer3 amount from to = do
  from_balance <- readTVar from
  if (amount <= from_balance)
     then writeTVar from (from_balance - amount) >> (readTVar to >>= writeTVar to . (amount +))
     else return ()


-- | Wrapper for Person-based transactions
maketransfer :: (Dollars -> Account -> Account -> STM ()) -> Dollars -> Person -> Person -> STM ()
maketransfer transfertype amount person1 person2 = do
  transfertype amount (account person1) (account person2)


-- | test it out in ghci> atomically transferTest    
transferTest = do
  rbal <- newTVar (13 :: Dollars)
  let raf = Person {account=rbal}
  ebal <- newTVar (10 :: Dollars)
  let erin = Person {account=ebal}
  maketransfer transfer3 3 raf erin
  -- | maketransfer transfer2 3 raf erin
  -- | maketransfer transfer3 3 raf erin
  liftM2 (,) (readTVar rbal) (readTVar ebal)





-- | To help us read some of the binding syntax
-- 
--   (>>) 
--   sequences two actions together: the first is performed, then the second.  Controls evaluation.
--   ONLY one value is returned: the value of the second action, like how pure Haskell returns
--   one and only one value for each function
--   (>>) :: (Monad m) => m a -> m b -> m b
-- 
--   (>>=)
--   runs an action, then passes its result to a function that returns an action
--   so, also two actions, the first and then the function does the second
--   again ONLY one value is returned: the value of the second action, by the function
--  
--   putting these together
--   main = do
--   putStrLn "What's your first name?"
--   inputStr <- getLine
--   putStrLn $ "Welcome to STM, " ++ inpStr ++ "!"
-- 
--   main = 
--   putStrLn "What is your first name?"
--   getLine >>= (\inpStr -> putStrLn $ "Welcome to STM, " ++ inpStr ++ "!"

-- 
-- | return
--   this function in many other languages aborts execution of a function immediately and gives
--   a value to the caller
--   NOT SO in Haskell : return is used to wrap data in a monad.  For example in IO, return is used
--   to take pure data and bring it into the IO monad. Awesome!


