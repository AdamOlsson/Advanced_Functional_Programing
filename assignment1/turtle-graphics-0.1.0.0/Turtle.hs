{-# LANGUAGE GADTs #-}

-- | proper module documentation here
module Turtle (

  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation
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

  -- * Colors
  , blue
  , green
  , red

  -- * Run functions
  , run

  ) where 

-- | Exclude standard def of Right and Left and use our own definition
import Prelude hiding (Right, Left)
import qualified Graphics.HGL as HGL
import TurtleState
type Colour = HGL.Color


data Program where
  Forward   :: Double -> Program 
  Backward  :: Double -> Program
  Move      :: Double -> Program
  Right     :: Double -> Program 
  Left      :: Double -> Program
  Rotate    :: Double -> Program 
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
  Counter   :: Int -> Program -> Program
  Parallel  :: Program -> Program -> Program


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

(<|>) :: Program -> Program -> Program
(<|>) = Parallel

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


-- | Blue color
blue :: HGL.Color
blue = HGL.Blue

-- | Red color
red :: HGL.Color
red = HGL.Red

-- | Green color
green :: HGL.Color
green = HGL.Green


-- | Print the textual interface. 
runTextual :: Program -> TurtleState -> TurtleState -> IO ()
runTextual (Move   d) st st' = 
  putStrLn $ "Turtle took " ++ show d ++ " steps " ++ bf ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'
    bf = if d > 0 then "forward" else "backward"

runTextual (Rotate   d) st st' = 
  putStrLn $ "Turtle rotate " ++ show (abs d) ++ " degree/s " ++ lr ++
  " from " ++ show a ++ " to " ++ show b ++ "."
  where
    a = getAngle st
    b = getAngle st'
    lr = if a < 0 then "left" else "right"

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
runTextual (Lifespan  n p) _ _ = putStrLn $ "Runs the program for " ++
                                            show n ++ " timeunits then dies."
runTextual (Limited n p) _ _ = putStrLn $ "Runs the program for " ++
                                            show n ++ " more timeunits " ++
                                            "then continues."
runTextual (Counter n p) _ _ = putStrLn $ "Runs for " ++ show n ++ 
                                          " more timesteps."

{- | A program consist of a drawing window, turtle state and a 
bool keeping track of early termination-}
type ProgramState = (HGL.Window, [TurtleState], Bool, Int)

-- |Draw function that does not wait for the event tick.
drawNoTick :: ProgramState -> TurtleState -> IO()
drawNoTick (w, (st:sts), _, _) st' = case getDrawing st of 
  False -> return ()
  _     -> HGL.drawInWindow w $ HGL.withColor c $ HGL.line (round(x), round(y))
                                                         (round(x'), round(y'))
           where
            c         = getColor st
            (x, y)    = getPos st  
            (x', y')  = getPos st' 

-- | Helper run function
runProgram :: Program -> ProgramState -> IO ProgramState
runProgram (Seq p1 p2) (w, (st:sts), t, l) = case (t, l < 1) of
  (True,_) -> return (w, [st], t, l)
  (_, True)-> runProgram p1 (w, [st], t, 0) >>= runProgram p2
  _        -> runProgram p1 (w, [st], t, l) >>= runProgram (Counter (l-1) p2)

runProgram (Counter   n p) (w, (st:sts), t, l) = case n < 1 of 
  True -> return (w, [st], t, 0)
  _    -> runTextual (Counter n p) st st >>
          runProgram p (w, [st], t, n)

runProgram (Lifespan n p) (w, (st:sts), t, l) =  case n < 1 of 
  True -> return (w, [st], t, 0)
  _    -> runTextual (Lifespan n p) st st >>
          runProgram p (w, [st], t, n) >> runProgram die (w, [st], t, 0)

runProgram (Forward d)  st = runProgram (Move   d ) st
runProgram (Backward d) st = runProgram (Move (-d)) st
runProgram (Move d) (w, (st:sts), t, l) = do
  runTextual (Move d) st st'
  drawNoTick (w, (st:sts), t, l) st'
  return (w, [st'], t, l)
  where
    (x, y) = getPos st
    x' = x - sin(getAngle st)*d
    y' = y - cos(getAngle st)*d
    st'     = updatePos st (x',y')

runProgram (Right d) st = runProgram (Rotate (-d)) st
runProgram (Left  d) st = runProgram (Rotate   d ) st
runProgram (Rotate d) (w, (st:sts), t, l) = do
  runTextual (Rotate d) st st'
  return (w, [st'], t, l)
  where
    a  = getAngle st
    a' = a + d
    st' = updateAngle st a'

runProgram (Color c) (w, (st:sts), t, l) = do
  runTextual (Color c) st st'
  return (w, [st'], t, l)
  where
    c'  = getColor st
    st' = updateColor st c

runProgram (Forever p) (w, (st:sts), t, l)    = runTextual (Forever p) st st >>
                                          runProgram
                                          ((>*>) p (Forever p)) (w, [st], t, l)

runProgram  Penup    (w, (st:sts), t, l)      = runTextual Penup   st st >>
                                          return (w, (toggleDrawing st : sts), t, l)
runProgram  Pendown  (w, (st:sts), t, l)      = runTextual Pendown st st >>
                                          return (w, (toggleDrawing st : sts), t, l)
runProgram  Idle (w, (st:sts), t, l)          = runTextual Idle    st st >>
                                          return (w, [st], t, l)
runProgram (Times n p) (w, (st:sts), t, l) = case n <= 0 of
                                         True-> return (w, [st], t, l)
                                         _   -> runTextual (Times n p) st st >>
                                          runProgram 
                                          ((>*>) p (Times (n-1) p)) 
                                           (w, [st], t, l)
runProgram  Die (w, (st:sts), t, l)           = runTextual Die st st >>
                                          return (w, [st], True, l)
                                          
runProgram (Limited   n p) (w, (st:sts), t, l) = case n < 1 of 
  True -> return (w, [st], t, 0)
  _    -> runTextual (Limited n p) st st >>
          runProgram p (w, [st], t, n)

      



  --  do
  -- let s1 = runProgram step1 (w, (st:sts), t, l)
  -- where
  --   step1 = chop p1
  --   step2 = chop p2
  -- step p1 st -> return new state
  -- step p2 newState -> return new state 2

  {-
  1. Create new turtle with new id, same pos
  2. Add Turtle
  3. Step
  -}

-- step :: Program -> ProgramState -> IO ProgramState
-- step (Seq p1 p2) (w, (st:sts), t, l) = do
--   return 

chop :: Program -> Program
chop (Seq p1 p2) = p1
chop p           = p


{- Question: What definition of time do you use (what can a turtle achieve in
   a single time unit)?
We define that one action is going to take 1 timeunit to complete.
-}

-- | Entry point (run function)
run :: Program -> IO ()
run p = HGL.runGraphics $ do
    w <- HGL.openWindowEx "Turtle!" Nothing (720, 640) HGL.DoubleBuffered (Just 1000)
    HGL.drawInWindow w (HGL.polygon [(0,0),(0,640),(720,640),(720,0)])
    runProgram p (w, [newTurtleState], False, 0)
    HGL.getKey w >> return ()
