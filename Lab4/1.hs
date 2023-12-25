data MyBool = T | F (Show)

myand T T = T
myand _ _ = F

myor F F = F
myor _ _ = T

mynot F = T
mynpt T = F

