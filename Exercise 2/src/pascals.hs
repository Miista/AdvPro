pascals :: Int -> [Int] -> [Int]
pascals 0 l = l
pascals n list = pascals (n-1) $ zipWith (+) l r
             where l = 0 : list
                   r = list ++ [0]

-- Data Structure List
data List a = Nil | Cons a (List a) deriving (Show)

lSetHead :: a -> List a -> List a
lSetHead a list = Cons a list

tailL :: List a -> List a
tailL Nil = Nil
tailL (Cons _ tail) = tail

dropL :: Int -> List a -> List a
dropL 0 list = list
dropL n list = dropL (n-1) (tailL list)

lLength :: List a -> Int
lLength Nil = 0
lLength (Cons _ tail) = 1 + (lLength tail)

lFilter :: (a -> Bool) -> List a -> List a
lFilter _ Nil = Nil
lFilter f (Cons a tail) =
    if f(a)
    then (Cons a (lFilter f tail))
    else lFilter f tail

dropLWhile :: List a -> (a -> Bool) -> List a
dropLWhile Nil _ = Nil
dropLWhile list@(Cons a tail) f =
    if f(a)
    then dropLWhile tail f
    else list

lInit :: List a -> List a
lInit Nil = Nil
lInit (Cons a Nil) = Nil
lInit (Cons a tail) = Cons a (lInit tail)

foldRight :: List a -> (a -> b) -> List b
foldRight Nil _ = Nil
foldRight (Cons a tail) f = Cons (f a) (foldRight tail f)

lZipWith :: List a -> List b -> (a -> b -> c) -> List c
lZipWith Nil _ _ = Nil
lZipWith _ Nil _ = Nil
lZipWith (Cons l lt) (Cons r rt) f = Cons (f l r) (lZipWith lt rt f)

mkList = Cons 1 (Cons 2 (Cons 3 Nil))
