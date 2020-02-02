{-# LANGUAGE GADTs #-}

-- | proper module documentation here
module Turtle (

  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program

  -- * Primitive operations
  , forward, right, left, backward
  , (>*>)
  , pendown, penup
  , color
  , die
  , idle
  , limited
  , lifespan
  , times
  , forever

  -- * Derived operations
  -- ...

  -- * Run functions
  , runTextual
  , run

  ) where 

-- | Exclude standard def of Right and Left and use our own definition
import Prelude hiding (Right, Left)
import qualified Graphics.HGL as HGL

import TurtleGraphics.Logger
import TurtleGraphics.State
import TurtleGraphics.TurtleState


type Colour = (Int, Int, Int)


--   You can use newtype instead of data if you wish.
data Program where
  Forward   :: Double -> Program 
  Backward  :: Double -> Program 
  Right     :: Double -> Program 
  Left      :: Double -> Program 
  Penup     :: Program           
  Pendown   :: Program           
  Color     :: Colour -> Program  
  Die       :: Program           
  Idle      :: Program           
  Limited   :: Int -> Program -> Program 
  Lifespan  :: Int -> Program -> Program 
  Times     :: Int -> Program -> Program    
  Forever   :: Program -> Program        
  Seq       :: Program -> Program -> Program 

-- data Log a = Log (a, [String])

-- instance Monad Log where
--   return x = Log (x, [])
--   Log (x,msg) >>= f = case f x of
--                           Log (x, msg') -> Log (x, msg ++ msg')

-- log :: String -> Log ()
-- log m = Log ((), [m])

-- | Move a distance forward
forward :: Double -> Program
forward = Forward

-- | Move a distance backward
backward :: Double -> Program
backward = Backward

-- | Turn degrees right
right :: Double -> Program
right = Right

-- | Turn degrees left
left :: Double -> Program
left = Left

-- | Stop drawing
penup :: Program
penup = Penup

-- | Start drawing
pendown :: Program
pendown = Pendown

-- | Set drawing color
color :: Colour -> Program
color = Color

-- | Stop the Program, no more instruction will be executed
die :: Program
die = Die

-- | Wait
idle :: Program
idle = Idle

-- | Execute a progrum until time runs out
limited :: Int -> Program -> Program
limited = Limited

-- | Kill turtle after a time
lifespan :: Int -> Program -> Program  
lifespan = Lifespan

-- | Execute a Program an amount of times
times :: Int -> Program -> Program  
times = Times  

-- | Repeat Program forever
forever :: Program -> Program
forever = Forever

-- | Sequence two programs after each other
(>*>) :: Program -> Program -> Program
(>*>) = Seq 

-- | Draws a spiral
spiral :: Double -> Double -> Program
spiral size angle 
  | size > 100 = die
  | otherwise  = (>*>) (forward size) $
                 (>*>) (right angle) (spiral (size+2) angle)

-- | A spiral that will continue forever
spiralForever :: Double -> Double -> Program
spiralForever size angle =  (>*>) (forward size) $
                            (>*>) (right angle) (spiralForever (size+2) angle)

{-| Q. Can you define a limited version in terms of the unlimited one?
Yes, but I am assuming that the time 100 is somewhat equivalent
to the 'size > 100' in the original implementation.
-}
spiral' :: Double -> Double -> Program
spiral' size angle = limited 100 $ spiralForever size angle

-- | A spiral that starts finite and becomes infinite
spiralFiniteThenInfite :: Double -> Double -> Program
spiralFiniteThenInfite size angle   
  | size > 100 = spiralForever size angle
  | otherwise  = (>*>) (forward size) $
                 (>*>) (right angle) (spiral (size+2) angle)


runTextual :: Program -> IO ()
runTextual (Forward   d)        = putStrLn $ "Forward "  ++
                                              show d ++ " steps."
runTextual (Backward  d)        = putStrLn $ "Backward " ++
                                              show d ++ " steps."
runTextual (Right     d)        = putStrLn $ "Right "    ++ 
                                              show d ++ " degree/s." 
runTextual (Left      d)        = putStrLn $ "Left "     ++
                                              show d ++ " degree/s."
runTextual Penup                = putStrLn $ "Stops drawing."          
runTextual Pendown              = putStrLn $ "Starts drawing."
runTextual (Color (r,g,b))      = putStrLn $ "Switching to color (" ++
                                              show r ++
                                              "," ++ show g ++
                                              "," ++ show b ++ ")"   
runTextual Die                  = putStrLn $ "Kill the turtle :'("
runTextual Idle                 = putStrLn $ "Idle..."          
runTextual (Limited   n p)      = putStrLn $ "Runs a program for " ++
                                              show n ++ " limited time."
runTextual (Lifespan  n p)      = putStrLn $ "Runs a program for the " ++
                                              "lifespan of " ++
                                              show n ++ " time."
runTextual (Times     n p)      = putStrLn $ "Runs program " ++
                                              show n ++ " times."
runTextual (Forever   p)        = putStrLn $ "Runs program forever."  



runProgram :: Program -> State TurtleState Program
runProgram (Seq p1 p2) = undefined
runProgram (Forward d) = do
  (TurtleState p a c is_drawing) <- getState
  -- Do something with progam -- What? Should probably draw with HGL here.
  -- How do I write on the same canvas, should it be in a state as well?
  -- put (updatePos s d) -- update the position of the state
  return (Forward d) -- What should be returned???

-- run (Backward  d)       = undefined
-- run (Right     d)       = undefined 
-- run (Left      d)       = undefined 
-- run  Penup              = undefined          
-- run  Pendown            = undefined          
-- run (Color (RGB r g b)) = undefined  
-- run  Die                = undefined          
-- run  Idle               = undefined          
-- run (Limited   n p)     = undefined
-- run (Lifespan  n p)     = undefined
-- run (Times     n p)     = undefined    
-- run (Forever   p)       = undefined


-- | Run function
run :: Program -> IO()
run p         = do
  putStrLn "Whatever logging should be happening"

  where MkState f = runProgram p
        (result, final_st) = f newTurtleState

