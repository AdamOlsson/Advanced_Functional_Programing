
module Examples where

import TurtleGraphics.Turtle


example1 :: Program
example1 = (>*>) (forward 100.0) 
            $ (>*>) (right 90) 
            $ (>*>) penup 
            $ (>*>) (forward 100.0) 
            $ (>*>) (left 90) 
            $ (>*>) pendown (forward 100.0)
