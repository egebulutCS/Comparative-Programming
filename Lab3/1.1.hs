addOneToAll :: [Integer] -> [Integer]
addOneToAll (x:y) = (x + 1) : addOneToAll (y)