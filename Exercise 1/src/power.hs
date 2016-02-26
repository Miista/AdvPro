mult a b = a * b

power :: Double -> Int -> Double
power _ 0 = 1
power x n
   | n < 0 = 1 / (power x $ abs n)
   | even n = mult (power x $ div n 2) $ power x $ div n 2
   | otherwise = mult x $ power x $ n-1

c1 :: Int -> Int
c1 a = a+1

c2 :: Int -> Int
c2 a = a*2

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g = \a -> g (f a)