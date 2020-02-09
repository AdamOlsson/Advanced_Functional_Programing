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

data Replay q r a 
  where
  Return    :: a -> Replay q r a
  Bind      :: Replay q r a -> (a -> Replay q r b) -> Replay q r b
  -- Io        ::
  -- Ask       ::
  -- AddAnswer ::

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
io = undefined

ask :: q -> Replay q r r
ask = undefined

emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r] 

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined