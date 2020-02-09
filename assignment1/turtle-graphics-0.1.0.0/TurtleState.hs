
module TurtleState where

import qualified Graphics.HGL as HGL

-- | Point on the canvas
type Point = (Double, Double)
-- | Angle of the facing direction of the turtle
type Angle = Double
-- | Drawing color, RGB
type Color = HGL.Color

data TurtleState = TurtleState {
    pos       :: Point,
    angle     :: Angle,
    c         :: Color,
    isDrawing :: Bool
}

showP :: (Double, Double) -> [Char]
showP (x, y) = "(" ++ show (round(x)) ++ ", " ++ show (round(y)) ++ ")"

showSt :: TurtleState -> IO()
showSt (TurtleState pos ang c b) = putStrLn $
                                    "(Pos: " ++ showP pos ++
                                    ", Angle: " ++ show ang ++
                                    ", Color: " ++ show c ++
                                    ", Drawing: " ++ show b

newTurtleState :: TurtleState
newTurtleState = TurtleState {  pos         = (200,200),
                                angle       = 45.0,
                                c           = HGL.Red,
                                isDrawing   = True
                            }

toggleDrawing :: TurtleState -> TurtleState
toggleDrawing (TurtleState p a c d) = TurtleState p a c (not d)

updateColor :: TurtleState -> HGL.Color -> TurtleState
updateColor (TurtleState p a _ d) c = TurtleState p a c d

updatePos :: TurtleState -> Point -> TurtleState
updatePos (TurtleState _ a c d) p = TurtleState p a c d

updateAngle :: TurtleState -> Angle -> TurtleState
updateAngle (TurtleState p _ c d) a = TurtleState p a c d

getPos :: TurtleState -> Point
getPos (TurtleState p _ _ _) = p

getAngle :: TurtleState -> Angle
getAngle (TurtleState _ a _ _) = a

getColor :: TurtleState -> HGL.Color
getColor (TurtleState _ _ c _) = c

getDrawing :: TurtleState -> Bool
getDrawing (TurtleState _ _ _ d) = d

roundPoint :: Point -> (Int, Int)
roundPoint (x, y) = (round(x), round(y))