first :: Int -> [a] -> [a]
first n list = foldl func [] list
    where    
    func y x | (length y) < n = x : y 
             | otherwise      = y
			 
first n (h:t) = if n==0 then [] else h:first (n-1) t

filt n [] = []
filt n (h:t) = if h 'mod' n == 0 then filt n t else h:filt n t

primes (h:t) = h:primes(filt h t)