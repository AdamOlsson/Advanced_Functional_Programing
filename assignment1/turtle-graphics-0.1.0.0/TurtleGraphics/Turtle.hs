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

  -- * Run functions
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


-- | Print the textual interface. 
runTextual :: Program -> TurtleState -> TurtleState -> IO ()
runTextual (Forward   d) st st' = 
  putStrLn $ "Turtle took " ++ show d ++ " steps forward" ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'

runTextual (Backward   d) st st' = 
  putStrLn $ "Turtle took " ++ show d ++ " steps backward" ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'

runTextual (Right   d) st st' = 
  putStrLn $ "Turtle rotate " ++ show d ++ " degree/s right" ++
  " from " ++ show a ++ " to " ++ show b ++ "."
  where
    a = getAngle st
    b = getAngle st'

runTextual (Left   d) st st' = 
  putStrLn $ "Turtle rotate " ++ show d ++ " degree/s left" ++
  " from " ++ show a ++ " to " ++ show b ++ "."
  where
    a = getAngle st
    b = getAngle st'

runTextual (Color   _) st st' = 
  putStrLn $ "Turtle switch color from " ++ show a ++
  " to " ++ show b ++ "."
  where
    a = getColor st
    b = getColor st'
runTextual (Times n p)    _ _ = putStrLn $ "Running program " ++ show n ++
                                            " times."
runTextual Pendown        _ _ = putStrLn $ "Turtle started drawing."
runTextual Penup          _ _ = putStrLn $ "Turtle stopped drawing."
runTextual (Forever p)    _ _ = putStrLn $ "Turtle runs the program forever."
runTextual Idle           _ _ = putStrLn $ "Turtle is idle..."
runTextual Die            _ _ = putStrLn $ "Turtle dies :'("
runTextual (Limited  n p) _ _ = putStrLn $ "Runs the program for " ++
                                            show n ++ " more timeunits then dies."
runTextual (Lifespan n p) _ _ = putStrLn $ "Runs the program for " ++
                                            show n ++ " more timeunits then continues."


-- | A program consist of a drawing window, turtle state and a bool keeping track of early termination
type ProgramState = (HGL.Window, TurtleState, Bool)

-- |Draw function that does not wait for the event tick.
drawNoTick :: ProgramState -> TurtleState -> IO()
drawNoTick (w, st, _) st' = case getDrawing st of 
  False -> return ()
  _     -> HGL.drawInWindow w $ HGL.withColor c $ HGL.line (round(x), round(y)) (round(x'), round(y'))
           where
            c         = getColor st
            (x, y)    = getPos st  
            (x', y')  = getPos st' 

-- | Helper run function
runProgram :: Program -> ProgramState -> IO ProgramState
runProgram (Seq p1 p2) s = runProgram p1 s >>= runProgram p2
runProgram (Forward d) (w, st, t) = do
  runTextual (Forward d) st st'
  drawNoTick (w, st, t) st'
  return (w, st', t)
  where
    (x, y)  = getPos st
    x'      = x + sin(getAngle st)*d
    y'      = y + cos(getAngle st)*d
    st'     = updatePos st (x',y')
    
runProgram (Backward d) (w, st, t) = do
  runTextual (Backward d) st st'
  drawNoTick (w, st, t) st'
  return (w, st', t)
  where
    (x, y) = getPos st
    x' = x - sin(getAngle st)*d
    y' = y - cos(getAngle st)*d
    st'     = updatePos st (x',y')

runProgram (Right d) (w, st, t) = do
  runTextual (Right d) st st'
  return (w, st', t)
  where
    a  = getAngle st
    a' = a - d
    st' = updateAngle st a'

runProgram (Left d) (w, st, t) = do
  runTextual (Left d) st st'
  return (w, st', t)
  where
    a  = getAngle st
    a' = a + d
    st' = updateAngle st a'

runProgram (Color c) (w, st, t) = do
  runTextual (Color c) st st'
  return (w, st', t)
  where
    c'  = getColor st
    st' = updateColor st c

runProgram (Forever p) (w, st, t)    =   runTextual (Forever p) st st >>
                                         runProgram
                                         ((>*>) p (Forever p)) (w, st, t)
runProgram  Penup    (w, st, t)      =   runTextual Penup   st st >>
                                         return (w, toggleDrawing st, t)
runProgram  Pendown  (w, st, t)      =   runTextual Pendown st st >>
                                         return (w, toggleDrawing st, t)
runProgram  Idle (w, st, t)          =   runTextual Idle    st st >>
                                         return (w, st, t)
runProgram (Times n p) (w, st, t)    =   case n < 1 of
                                          True -> runProgram p (w, st, t)
                                          _    -> 
                                            runTextual (Times n p) st st >>
                                            runProgram 
                                            ((>*>) p (Times (n-1) p)) (w, st, t)
runProgram  Die (w, st, t)           =   runTextual Die st st >>
                                         return (w, st, True)
runProgram (Lifespan n p) (w, st, t) =   runTextual (Lifespan n p) st st >>
                                         HGL.getWindowTick w >>
                                         case n < 1 of
                                           True -> runProgram Die (w, st, t)
                                           _    -> runProgram 
                                                   ((>*>) p (Lifespan (n-1) p)) (w, st, t)
runProgram (Limited   n p) (w, st, t)=  runTextual (Limited n p) st st >>
                                        HGL.getWindowTick w >>
                                        case n < 1 of
                                          True -> runProgram p (w, st, t)
                                          _    -> runProgram 
                                                  ((>*>) p (Limited (n-1) p)) (w, st, t)


-- | Entry point (run function)
run :: Program -> IO ()
run p = HGL.runGraphics $ do
    w <- HGL.openWindowEx "Turtle!" Nothing (720, 640) HGL.DoubleBuffered (Just 1000)
    HGL.drawInWindow w (HGL.polygon [(0,0),(0,640),(720,640),(720,0)])
    runProgram p (w, newTurtleState, False)
    HGL.getKey w >> return ()