
module TurtleGraphics.TurtleState where

-- | Point on the canvas
type Point = (Int, Int)
-- | Angle of the facing direction of the turtle
type Angle = Double
-- | Drawing color, RGB
type Color = (Int, Int, Int)

data TurtleState = TurtleState {
    pos       :: Point,
    angle     :: Angle,
    c         :: Color,
    isDrawing :: Bool
}

newTurtleState :: TurtleState
newTurtleState = TurtleState {  pos         = (0,0),
                                angle       = 90.0,
                                c           = (200, 0, 0),
                                isDrawing   = True
                            }

updateDrawing :: TurtleState -> Bool -> TurtleState
updateDrawing (TurtleState p a c _) d = TurtleState p a c d

updateColor :: TurtleState -> Color -> TurtleState
updateColor (TurtleState p a _ d) c = TurtleState p a c d

updatePos :: TurtleState -> Point -> TurtleState
updatePos (TurtleState _ a c d) p = TurtleState p a c d

updateAngle :: TurtleState -> Angle -> TurtleState
updateAngle (TurtleState p _ c d) a = TurtleState p a c d
