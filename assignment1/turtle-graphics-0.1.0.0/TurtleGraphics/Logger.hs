
module TurtleGraphics.Logger where

import Control.Applicative

newtype Logger a = Logger (a, [String])

instance Monad Logger where
    return x = Logger (x, [])
    Logger (x, m) >>= f = case f x of
                            Logger (x, m') -> Logger(x, m ++ m')


log :: String -> Logger ()
log m = Logger ((), [m])


instance Functor Logger where
   fmap f m = m >>= \a -> return (f a)

instance Applicative Logger where
   pure  = return
   (<*>) a_f a_x = a_f >>= \f -> a_x >>= \x -> pure $ f x