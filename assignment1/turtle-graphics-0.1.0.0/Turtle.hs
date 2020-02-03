{-# LANGUAGE GADTs #-}

-- | proper module documentation here
module TurtleGraphics.Turtle (

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
import TurtleGraphics.TurtleState



type Colour = HGL.Color

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
runTextual (Color c)      = putStrLn $ "Switching to color " ++ show c  
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


-- | A program consist of a drawing window, turtle state and a bool keeping track of early termination
type ProgramState = (HGL.Window, TurtleState, Bool)

draw :: HGL.Window -> Bool -> HGL.Graphic -> IO()
draw _ False _  = return ()
draw w _ g      = do
  HGL.getWindowTick w
  HGL.drawInWindow w g
  return () 

runProgram :: Program -> ProgramState -> IO ProgramState
runProgram (Seq p1 p2) s = runProgram p1 s >>= runProgram p2
runProgram (Forward d) (w, (TurtleState (x,y) a c dr), t) = do
      -- runTextual (Forward d)
      putStrLn $ "Turtle moves from " ++ showP (x,y) ++ " to " ++ showP (x',y')
      draw w dr $ HGL.withColor HGL.Red   $ HGL.line  (round(x), round(y)) (round(x'), round(y'))
      return (w, (TurtleState (x',y') a c dr), t)
      where
        x' = x + sin(a)*d
        y' = y + cos(a)*d
runProgram (Backward d) (w, (TurtleState (x,y) a c dr), t) = do
      -- runTextual (Backward d)
      putStrLn $ "Turtle moves from " ++ showP (x,y) ++ " to " ++ showP (x',y')
      draw w dr $ HGL.withColor HGL.Red   $ HGL.line  (round(x), round(y)) (round(x'), round(y'))
      return (w, (TurtleState (x',y') a c dr), t)
      where
        x' = x - sin(a*pi)*d
        y' = y - cos(a*pi)*d

runProgram (Right d) (w, st, t)          = runTextual (Right d) >> return (w, updateAngle st (-d), t)
runProgram (Left  d) (w, st, t)          = runTextual (Left d)  >> return (w, updateAngle st   d , t)
runProgram  Penup    (w, st, t)          = runTextual Penup     >> return (w, toggleDrawing st   , t)           
runProgram  Pendown  (w, st, t)          = runTextual Pendown   >> return (w, toggleDrawing st   , t)   
runProgram (Color c) (w, st, t)          = runTextual (Color c) >> return (w, updateColor st c, t)
runProgram  Die (w, st, t)               = runTextual Die >> return (w, st, True) -- ??
runProgram  Idle (w, st, t)              = runTextual Idle >> return (w, st, t)          
-- runProgram (Limited   n p)     = undefined
-- runProgram (Lifespan  n p) st    = runTextual (Lifespan)
runProgram (Times     n p) st    = runTextual (Times n p) >> case n < 1 of
                                                                    True -> return st
                                                                    _ ->  runProgram p st >>=
                                                                          runProgram (Times (n-1) p)
runProgram (Forever   p)   st    = runProgram p st >>= runProgram (Forever p)



run :: Program -> IO ()
run p = HGL.runGraphics $ do
    w <- HGL.openWindowEx "Turtle!" Nothing (720, 640) HGL.DoubleBuffered (Just 1000)
    HGL.drawInWindow w (HGL.polygon [(0,0),(0,640),(720,640),(720,0)])
    runProgram p (w, newTurtleState, False)
    HGL.getKey w >> return ()

f :: Program
f = (>*>) (Forward 100.0) $ (>*>) (Right 90) $ (>*>) Penup $ (>*>) (Forward 100.0) $ (>*>) (Left 90) $ (>*>) Pendown (Forward 100.0) 

