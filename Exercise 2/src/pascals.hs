pas :: Int -> [Int] -> [Int]
pas 0 l = l
pas n list = pas (n-1) $ zipWith (+) l r
             where l = 0 : list
                   r = list ++ [0]