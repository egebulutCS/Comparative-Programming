h n = (\m -> n + m)
j = h 10

plus = uncurry (+)

curry g 3 4
uncurry f (3,4)