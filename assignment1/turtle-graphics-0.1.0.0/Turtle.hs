{-# LANGUAGE GADTs #-}

module Turtle (

  -- * The turtle type(s)
  Program
  , Turtle
  , TurtleProgram

  -- * Primitive operations
  , forward, right, left, backward
  , (>*>), (<|>)
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

import Prelude hiding (Right, Left)
import qualified Graphics.HGL as HGL
import TurtleState
type Colour = HGL.Color


data Program where
  Forward   :: Double -> Program 
  Right     :: Double -> Program 
  TogglePen :: Program           
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
backward d = Forward (-d)

-- | Turn degrees right
right :: Double -> Program
right d = Right d

-- | Turn degrees left
left :: Double -> Program
left d = Right (-d)

-- | Stop drawing
penup :: Program
penup = TogglePen

-- | Start drawing
pendown :: Program
pendown = TogglePen

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

-- | Run programs in parallel
(<|>) :: Program -> Program -> Program
(<|>) = Parallel


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
runTextual (Forward   d) st st' = 
  putStrLn $ "Turtle " ++ show id ++ " took " ++ show d ++ " steps " ++ fr ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'
    id = getId st
    fr = if d > 0 then "forward" else "backward"

runTextual (Right   d) st st' = 
  putStrLn $ "Turtle " ++ show id ++ " rotate " ++ show (abs d) ++
  " radians " ++ rl ++" from " ++ show a ++ " to " ++ show b ++ "."
  where
    a = getAngle st
    b = getAngle st'
    id = getId st
    rl = if d > 0 then "left" else "right"

runTextual (Color   _) st st' = 
  putStrLn $ "Turtle " ++ show id ++" switch color from " ++ show a ++
  " to " ++ show b ++ "."
  where
    a = getColor st
    b = getColor st'
    id = getId st

runTextual (Times n p) st _     = putStrLn $ "Turtle "++ (show $ getId st) ++
                                        " runs program "++ show n ++" times."
  
runTextual TogglePen st st'     = putStrLn $ "Turtle " ++ (show $ getId st)++
                                        " toggled drawing from " ++
                                        show (getDrawing st) ++ " to " ++
                                        show (getDrawing st')

runTextual (Forever p)     st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " runs the program forever."
runTextual Idle            st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " is idle..."
runTextual Die             st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " dies :'("
runTextual (Lifespan  n p) st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " runs the program for " ++
                                            show n ++ " timeunits then dies."
runTextual (Limited n p)   st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " runs the program for " ++
                                            show n ++ " more timeunits " ++
                                            "then continues."
runTextual (Counter n p)   st _  = putStrLn $ "Turtle " ++(show $ getId st)++
                                        " runs for " ++ show n ++
                                        " more timesteps."

{- | A Turtle consists of a program to execute,
a state, a boolean keeping track of early termination
and a counter for times operations-}
type Turtle = (Program, TurtleState, Bool, Int)
{- | A program consist of a drawing window and a
list of Turtles -}
type TurtleProgram = (HGL.Window, [Turtle])

newTurtle :: Program -> Turtle
newTurtle p = (p, newTurtleState, False, 0) 

getProgram :: Turtle -> Program
getProgram (p, st, t, l) = p

getTurtleState :: Turtle -> TurtleState
getTurtleState (p, st, t, l) = st

getTime :: Turtle -> Int
getTime (p, st, t, l) = l

getTerminal :: Turtle -> Bool
getTerminal (p, st, t, l) = t

setTerminal :: Turtle -> Turtle
setTerminal (p, st, _, l) = (p, st, True, l)

updateTurtleWithState :: Turtle -> TurtleState -> Turtle
updateTurtleWithState (p, _, t, l) st = (p, st, t, l)

updateTurtleWithProgram :: Turtle -> Program -> Turtle
updateTurtleWithProgram (_, st, t, l) p = (p, st, t, l)

setTurtleTimer :: Turtle -> Int -> Turtle
setTurtleTimer (p, st, t, _) n = (p, st, t, n)

decrementTurtleTimer :: Turtle -> Turtle
decrementTurtleTimer (p, st, t, l) = (p, st, t, (l-1))

-- | Entry point (run function)
run :: Program -> IO ()
run p = HGL.runGraphics $ do
    w <- HGL.openWindowEx "Turtle!" Nothing (720, 640) HGL.DoubleBuffered (Just 1000)
    HGL.drawInWindow w (HGL.polygon [(0,0),(0,640),(720,640),(720,0)])
    runProgram (w, [newTurtle p])
    HGL.getKey w >> return ()

-- | Helper run function
runProgram :: TurtleProgram -> IO TurtleProgram
runProgram (w, [])      = do
  return (w, [])
runProgram (w, (t:ts))  = case program of
  Seq p1 p2 -> case (getTerminal t, (getTime t) < 1) of
    (True, _) -> runProgram (w, ts)
    (_, True) -> do
      let t1 = updateTurtleWithProgram t p1
      (w1, (t2:ts1)) <- runProgram (w, ([t1]))
      let t3 = updateTurtleWithProgram t2 p2
      runProgram (w1, ts ++ [t3])

    _         -> do
      let t1 = updateTurtleWithProgram t p1
      (w1, (t2:ts1)) <- runProgram (w, (t1:ts))
      let t3 = updateTurtleWithProgram t2 (Counter ((getTime t2)-1) p2)
      let t4 = decrementTurtleTimer t3
      runProgram (w1, ts1 ++ [t4])

  Counter n p -> do
    case n < 1 of 
      True -> return (w, (t:ts))
      _    -> do
        runTextual (Counter n p) st st
        let t1 = updateTurtleWithProgram t p
        runProgram (w, t1:ts)
        where
          st = getTurtleState t

  Forward d -> do
    runTextual program st st'
    draw w st st'
    return (w, (t':ts))
    where
      st    = getTurtleState t
      (x,y) = getPos st
      x'    = x + sin(getAngle st)*d
      y'    = y + cos(getAngle st)*d
      st'   = updatePos st (x',y')
      t'    = updateTurtleWithState t st'

  Right d -> do
    runTextual program st st'
    draw w st st'
    return (w, t':ts)
    where
      st    = getTurtleState t
      a     = getAngle st
      a'    = a + d
      st'   = updateAngle st a'
      t'    = updateTurtleWithState t st'

  Color c -> do
    runTextual program st st'
    return (w, t1:ts)
    where
      st = getTurtleState t
      c'  = getColor st
      st' = updateColor st c
      t1  = updateTurtleWithState t st'

  TogglePen -> do
    runTextual program st st'
    return (w, t1:ts)
    where
      st  = getTurtleState t
      st' = toggleDrawing st
      t1  = updateTurtleWithState t st'

  Idle -> do
    runTextual program newTurtleState newTurtleState
    return (w, t:ts)

  Die -> do
    runTextual program st st
    return (w, t1:ts)
    where
      t1 = setTerminal t
      st = getTurtleState t1

  Times n p -> case n <= 0 of
    True  -> return (w, ts)
    _     -> do
      runTextual program st st
      runProgram (w, t1:ts)
      where
        t1 = updateTurtleWithProgram t ((>*>) p $ Times (n-1) p)
        st = getTurtleState t
  
  Forever p -> do
    runTextual program st st
    runProgram (w, t1:ts)
    where
      t1 = updateTurtleWithProgram t ((>*>) p $ Forever p)
      st = getTurtleState t

  Limited n p -> case n < 1 of
    True  -> return (w, ts)
    _     -> do
      runTextual program st st
      runProgram (w, t2:ts)
      where
        t1 = setTurtleTimer t n
        t2 = updateTurtleWithProgram t1 p
        st = getTurtleState t

  Lifespan n p -> case n < 1 of
    True  -> return (w, ts)
    _     -> do
      runTextual program st st
      runProgram (w, t2:ts)
      runProgram (w, t3:ts)
      where
        t1 = updateTurtleWithProgram t p
        t2 = setTurtleTimer t1 n
        t3 = updateTurtleWithProgram t2 Die
        st = getTurtleState t

  Parallel p p' -> do
    runParallel (w, (t1:t3:ts))
    return (w, []) 
    where
      t1 = updateTurtleWithProgram t p
      t2 = updateTurtleWithProgram t p'
      st = getTurtleState t2
      id = getNextId (t:ts) 0
      st' = setId st id
      t3 = updateTurtleWithState t2 st'

  where
    program = getProgram t

-- | Draw function that does not wait for the event tick.
draw :: HGL.Window -> TurtleState -> TurtleState -> IO()
draw w st st'=  case getDrawing st of 
  False -> return ()
  _     -> HGL.drawInWindow w $ HGL.withColor c
                              $ HGL.line (round(x) , round(y ))
                                         (round(x'), round(y'))
    where
      c         = getColor st
      (x, y)    = getPos st  
      (x', y')  = getPos st'

{- | Initiates programs runnning in parallel by putting
more states in the list of states.-}
runParallel :: TurtleProgram -> IO TurtleProgram
runParallel (w, [])     = return (w, [])
runParallel (w, ts) = do
  (w, ts1) <- runProgram (w, ts)
  case ts1 of
    [] -> return (w, [])
    (t1:ts1') -> runParallel (w, ts1')

-- | Finds the next id for a new turtle
getNextId :: [Turtle] -> Int -> Int
getNextId [] max_id     = max_id+1
getNextId (t:ts) max_id = getNextId ts next
  where
    st = getTurtleState t
    id = getId st
    next = if id > max_id then id else max_id
