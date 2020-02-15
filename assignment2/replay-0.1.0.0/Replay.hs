{-# LANGUAGE GADTs #-}

module Replay
  (
    Replay

  , io
  , ask
  , emptyTrace
  , addAnswer
  , run
  ) where

import Control.Monad (ap)
import Data.Time (getCurrentTime, diffUTCTime)


data Replay q r a where
  Return :: a -> Replay q r a
  Bind   :: Replay q r a -> (a -> Replay q r b) -> Replay q r b
  IO     :: IO a -> Replay q r a
  Ask    :: q -> Replay q r r


instance Monad (Replay q r) where
  return = Return
  (>>=)  = Bind

instance Applicative (Replay q r) where
  pure  = return
  (<*>) = ap

instance Functor (Replay q r) where
  fmap f x = pure f <*> x

-- data Trace r
type Trace r = [Item r]

data Item r = Answer r | Result String
  deriving (Show, Read)

io :: (Show a, Read a) => IO a -> Replay q r a
io = IO

ask :: q -> Replay q r r
ask  = Ask 

emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]

addResult :: Trace r -> String -> Trace r
addResult t r = t ++ [Result r]


run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Bind replay f)  tr = do
  a <- run replay tr
  case a of
    Right x     -> run (f x) tr
    Left (q, t) -> return $ Left (q, t)

    
run (Return a)  tr = return $ Right a


run (IO action) tr = case tr of
  ((Result str):tr) -> ??
  _                 -> do
    res <- action
    return $ Left (??, tr ++ [Result res]


run (Ask quest) tr = case tr of
  ((Answer a):trs)  -> return $ Right a
  _                 -> return $ Left (quest, tr)



ex :: Replay String String Int
ex = do
  t0 <- io getCurrentTime
  return 1

p :: Replay String String Int
p = do 
  name <- ask "What is your name?"
  age <- ask "What is your age?"
  return (read age)

g :: Replay String String Int
g = do 
  io (putStrLn "Hello")
  return (read "dsd")

-- example :: Replay String String Int
-- example = do
  -- t0 <- io getCurrentTime
  -- io (putStrLn "Hello!")
  -- age <- ask "What is your age?" 
  -- io (putStrLn ("You are " ++ age))
  -- name <- ask "What is your name?"
  -- io (putStrLn (name ++ " is " ++ age ++ " years old"))
  -- t1 <- io getCurrentTime
  -- io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  -- return (read age)