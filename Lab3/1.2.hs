timesTwoToAll :: [Integer] -> [Integer]
timesTwoToAll (x:y) = (x * 2) : timesTwoToAll (y)