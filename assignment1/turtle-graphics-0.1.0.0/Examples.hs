
module Examples where

import Turtle2

spiral :: Double -> Double -> Program
spiral size angle 
  | size > 100 = die
  | otherwise  = (>*>) (forward size) $
                 (>*>) (right angle) (spiral (size+2) angle)

example1 :: Program
example1 = (>*>) (forward 100.0) 
            $ (>*>) (right (pi/2)) 
            $ (>*>) penup 
            $ (>*>) (forward 100.0)
            $ (>*>) (color blue)
            $ (>*>) (left (pi/2))
            $ (>*>) pendown (forward 100.0)


example2 :: Program
example2 = (>*>) (forward 100)
            $ (>*>) idle
            $ (>*>) (right 90)
            $ (>*>) (backward 50)
            $ (>*>) die
            $ (>*>) (right 10) (backward 100)

turnRight :: Program
turnRight = (>*>) (right 90) (forward 100) 

example3 :: Program
example3 =  (>*>) (times 4 turnRight) (forward 300)


spiralForever :: Double -> Double -> Program
spiralForever size angle =  (>*>) (forward size) $
                            (>*>) (right angle) (spiralForever (size+2) angle)

example4 :: Program
example4 = spiralForever 100.0 10.0

example5 :: Program
example5 = limited 100 example4

forward3 :: Program
forward3 = (>*>) (forward 50)
            $ (>*>) (forward 50) (forward 50)

example6 :: Program
example6 =    (>*>) (lifespan 4 forward3)
            $ (>*>) (forward 50) (forward 50)


main :: Program
main = example5


forward6 :: Program
forward6 = (>*>) forward3 forward3

forward12 :: Program
forward12 = (>*>) forward6 forward6

blueSquare :: Program
blueSquare =    (>*>) (color blue)
        $   (>*>) forward3
        $   (>*>) (left (pi/2))
        $   (>*>) forward3
        $   (>*>) (left (pi/2))
        $   (>*>) forward3
        $   (>*>) (left (pi/2)) forward3

redSquare :: Program
redSquare =    (>*>) (color red)
        $   (>*>) forward3
        $   (>*>) (right (pi/2))
        $   (>*>) forward3
        $   (>*>) die
        $   (>*>) (right (pi/2))
        $   (>*>) forward3
        $   (>*>) (right (pi/2)) forward3

parSquare :: Program
parSquare = (<|>) redSquare blueSquare 

