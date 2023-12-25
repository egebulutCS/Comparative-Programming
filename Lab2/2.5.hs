insertOrdered :: Ord a => a -> [a] -> [a]
insertOrdered a [] = [a]
insertOrdered a bs'@(b:bs)
 | a <= b = a : bs'
 | otherwise = b : insertOrdered a bs
 
insert x [] = [x]
inser x (h:t)
 | x <= h = x:h:t
 | otherwise h:insert x t