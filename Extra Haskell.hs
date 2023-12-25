findTail :: [Int] -> Int

findTail [t] = t
findTail (t:h) = findTail h

penultimate :: [Int] -> Int

penultimate (x:_:[]) = x
penultimate (_:xs) = penultimate xs

findGreater :: [Int] -> Int

findGreater [t] = t
findGreater (x:s:xs) = if x < s then findGreater (s:xs) else findGreater (x:xs)

findSmallest :: [Int] -> Int

findSmallest [t] = t
findSmallest (x:s:xs) = if x > s then findSmallest (s:xs) else findSmallest (x:xs)

findGS :: [Int] -> (Int,Int)

findGS t = (findSmallest t, findGreater t)

rmAdj :: Eq a => [a] -> [a]

rmAdj [s] = [s]
rmAdj (x:s:[]) = if x == s then rmAdj [s] else x : rmAdj [s]
rmAdj (x:s:xs) = if x == s then rmAdj (s:xs) else x : rmAdj (s:xs)

dup :: [a] -> [a]

dup [t] = t : t : []
dup (x:s) = x : x : dup s

duplicaten :: [a] -> Int -> [a]

duplicaten l@(x:xs) count = helper l count count 
  where helper [] _ _ = []
        helper (x:xs) count 0 = helper xs count count
        helper (x:xs) count n = x:(helper (x:xs) count (n-1))
        
dropn :: [a] -> Int -> [a]

dropn [] _ = []
dropn l@(x:xs) count = helper l count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x:(helper xs count (n-1))

change :: Int -> [Int] -> [Int]

change 0 [] = []
change 0 (x:xs) = 0:(change 0 xs)
change n l@(x:xs) = if n >= x && (n-x) >= x then [head (change (n-x) l) + 1] else if n >= x && (n-x) < x then 1:(change (n-x) xs) else 0:(change n xs)

getn :: [a] -> Int -> a

getn l@(x:xs) n = if n > length l then error "Out of index" else if n == 0 then x else getn xs (n-1)

getnC :: ([a],Int) -> a

getnC (l@(x:xs),n) = if n > length l then error "Out of index" else if n == 0 then x else getnC (xs,(n-1))

rotatel :: [a] -> [a]

rotatel (x:xs) = xs ++ [x]

rotater :: [a] -> [a]

rotater l = last l : init l

split :: [a] -> Int -> ([a],[a])

split [] _ = ([],[]) 
split l@(x:xs) n = if n > 0 then (x : ys, zs)  else ([],l) where (ys, zs) = split xs (n-1)

rotatern :: [a] -> Int -> [a]

rotatern [] _ = []
rotatern l@(x:xs) n = if n > 0 then rotatern (last l : init l) (n-1)  else l

rotateSplit :: [a] -> Int -> [a]

rotateSplit l@(x:xs) n = snd(split l (length l-n)) ++ fst(split l (length l-n))

nOfLowerF :: [Char] -> Int

nOfLowerF l = length (filter (`elem` ['a'..'z']) l)

nOfUpperF :: [Char] -> Int

nOfUpperF l = length (filter (`elem` ['A'..'Z']) l)