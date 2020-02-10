
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
    isDrawing :: Bool,
    tid       :: Int
}

showP :: (Double, Double) -> [Char]
showP (x, y) = "(" ++ show (round(x)) ++ ", " ++ show (round(y)) ++ ")"

showSt :: TurtleState -> IO()
showSt (TurtleState pos ang c b id) = putStrLn $
                                    "Pos: " ++ showP pos ++
                                    ", Angle: " ++ show ang ++
                                    ", Color: " ++ show c ++
                                    ", Drawing: " ++ show b ++
                                    ", id: " ++ show id

newTurtleState :: TurtleState
newTurtleState = TurtleState {  pos         = (200,200),
                                angle       = 45.0,
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