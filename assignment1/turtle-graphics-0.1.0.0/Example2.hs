module Example2 where
import Turtle2


f:: Program
f =   (>*>)(forward 100)
    $ (>*>)(right (pi/2)) (forward 100)

t :: Program
t = (<|>) (forward 100) $ (>*>) (right (pi/2)) (backward 300)