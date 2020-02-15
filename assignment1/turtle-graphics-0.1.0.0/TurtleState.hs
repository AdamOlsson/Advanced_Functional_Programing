
module TurtleState where

import qualified Graphics.HGL as HGL

-- | Point on the canvas
type Point = (Double, Double)
-- | Angle of the facing direction of the turtle
type Angle = Double
-- | Drawing color, RGB
type Color = HGL.Color

{- | Turtle state is a position,
facing angle, color, a boolean keeping
track if drawing should be done and a turtle id.-}
data TurtleState = TurtleState {
    pos       :: Point,
    angle     :: Angle,
    c         :: Color,
    isDrawing :: Bool,
    tid       :: Int
    }

-- | Show function for point
showP :: (Double, Double) -> [Char]
showP (x, y) = "(" ++ show (round(x)) ++ ", " ++ show (round(y)) ++ ")"

newTurtleState :: TurtleState
newTurtleState = TurtleState {  pos         = (350,300),
                                angle       = (pi/2),
                                c           = HGL.Red,
                                isDrawing   = True,
                                tid         = 0
                            }

toggleDrawing :: TurtleState -> TurtleState
toggleDrawing (TurtleState p a c d id) = TurtleState p a c (not d) id

updateColor :: TurtleState -> HGL.Color -> TurtleState
updateColor (TurtleState p a _ d id) c = TurtleState p a c d id

updatePos :: TurtleState -> Point -> TurtleState
updatePos (TurtleState _ a c d id) p = TurtleState p a c d id

updateAngle :: TurtleState -> Angle -> TurtleState
updateAngle (TurtleState p _ c d id) a = TurtleState p a c d id

getPos :: TurtleState -> Point
getPos (TurtleState p _ _ _ _) = p

getAngle :: TurtleState -> Angle
getAngle (TurtleState _ a _ _ _) = a

getColor :: TurtleState -> HGL.Color
getColor (TurtleState _ _ c _ _) = c

getDrawing :: TurtleState -> Bool
getDrawing (TurtleState _ _ _ d _) = d

roundPoint :: Point -> (Int, Int)
roundPoint (x, y) = (round(x), round(y))

getId :: TurtleState -> Int
getId (TurtleState _ _ _ _ id) = id

setId :: TurtleState -> Int -> TurtleState
setId (TurtleState p a c d _) id = (TurtleState p a c d id)