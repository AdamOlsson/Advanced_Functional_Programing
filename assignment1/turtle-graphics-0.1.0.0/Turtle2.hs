{-# LANGUAGE GADTs #-}

-- | proper module documentation here
module Turtle2 (

  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation
  Program

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
backward = Backward

-- | Turn degrees right
right :: Double -> Program
right = Right

-- | Turn degrees left
left :: Double -> Program
left = Left

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


-- -- | Print the textual interface. 
runTextual :: Program -> TurtleState -> TurtleState -> IO ()
runTextual (Forward   d) st st' = 
  putStrLn $ "Turtle " ++ show id ++ " took " ++ show d ++ " steps forward" ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'
    id = getId st

runTextual (Backward   d) st st' = 
  putStrLn $ "Turtle " ++ show id ++ " took " ++ show d ++ " steps backward" ++
  " from " ++ showP a ++ " to " ++ showP b ++ "."
  where
    a = getPos st
    b = getPos st'
    id = getId st

runTextual (Right   d) st st' = 
  putStrLn $ "Turtle rotate " ++ show (abs d) ++ " degree/s left" ++
  " from " ++ show a ++ " to " ++ show b ++ "."
  where
    a = getAngle st
    b = getAngle st'

runTextual (Left   d) st st' = 
  putStrLn $ "Turtle rotate " ++ show (abs d) ++ " degree/s right" ++
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
runTextual TogglePen st st' = putStrLn $ "Turtle toggled drawing from " ++
                                          show (getDrawing st) ++ " to " ++
                                          show (getDrawing st')

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
type Turtle = (Program, TurtleState, Bool, Int)
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

-- |Draw function that does not wait for the event tick.
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

printDebug :: Turtle -> IO()
printDebug t = putStrLn $ show p
  where 
    p = getTime t

runProgram :: TurtleProgram -> IO TurtleProgram
runProgram (w, [])      = return (w, [])
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

  Parallel p p' -> do
    runParallel (w, (t1:t3:ts))
    runProgram (w, []) -- Assume no Seq after parallel. (Which state should continue?)
    where
      t1 = updateTurtleWithProgram t p
      t2 = updateTurtleWithProgram t p' -- TODO Increase turtle id
      st = getTurtleState t2
      id = getNextId (t:ts) 0
      st' = setId st id
      t3 = updateTurtleWithState t2 st'

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

  Backward d -> do
    runTextual program st st'
    draw w st st'
    return (w, (t':ts))
    where
      st    = getTurtleState t
      (x,y) = getPos st
      x'    = x - sin(getAngle st)*d
      y'    = y - cos(getAngle st)*d
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

  Left d -> do
    runTextual program st st'
    draw w st st'
    return (w, t':ts)
    where
      st    = getTurtleState t
      a     = getAngle st
      a'    = a - d
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
    runTextual program newTurtleState newTurtleState
    return (w, t1:ts)
    where
      t1 = setTerminal t

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
        t3 = updateTurtleWithProgram t2 Die -- state does not matter
        st = getTurtleState t

  where
    program = getProgram t

runParallel :: TurtleProgram -> IO TurtleProgram
runParallel (w, [])     = return (w, [])
runParallel (w, ts) = do
  (w, (t1:ts1)) <- runProgram (w, ts)
  runParallel (w, ts1)




getNextId :: [Turtle] -> Int -> Int
getNextId [] max_id     = max_id+1
getNextId (t:ts) max_id = getNextId ts next
  where
    st = getTurtleState t
    id = getId st
    next = if id > max_id then id else max_id
