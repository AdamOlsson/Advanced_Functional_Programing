module TurtleGraphics.State where

{-
A monad keeping track of the state.
-}

data State s a = MkState (s -> (a,s))

getState :: State s s
getState = MkState $ \s -> (s,s)

putState :: s -> State s ()
putState s = MkState $ \_ -> ((), s)

instance Monad (State s) where
    return x        = MkState $ \s -> (x,s)
    (MkState m ) >>= k = MkState $ \s1 -> let (a, s2) = m s1
                                              MkState km = k a
                                          in km s2


instance Functor (State s) where
   fmap f m = m >>= \a -> return (f a)

instance Applicative (State s) where
   pure  = return
   (<*>) a_f a_x = a_f >>= \f -> a_x >>= \x -> pure $ f x
