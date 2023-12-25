fibonacci :: Integer -> Integer
fibonacci n
 | n > 1 = fibonacci(n - 1) + fibonacci(n - 2)
 | otherwise = 1