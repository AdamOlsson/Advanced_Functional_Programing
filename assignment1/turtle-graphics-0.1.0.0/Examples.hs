
module Examples where

import Turtle


example1 :: Program
example1 = (>*>) (forward 100.0) 
            $ (>*>) (right 90) 
            $ (>*>) penup 
            $ (>*>) (forward 100.0)
            $ (>*>) (color blue)
            $ (>*>) (left 90)
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