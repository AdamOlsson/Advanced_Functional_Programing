
module TurtleGraphics.TurtleState where

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
showP (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

showSt :: TurtleState -> IO()
showSt (TurtleState pos ang c _) = putStrLn $
                                    "(Pos: " ++ showP pos ++
                                    ", Angle: " ++ show ang ++
                                    ", Color: " ++ show c

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

updateAngle :: TurtleState -> Angle -> TurtleState
updateAngle (TurtleState p a c d) a' = TurtleState p (a+a') c (d)
