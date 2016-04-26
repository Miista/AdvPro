data Node a = Node2 a a 
            | Node3 a a a
            deriving (Show)

data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                  deriving (Show)

data Digit a = One a
             | Two a a
             | Three a a a
             | Four a a a a
             deriving (Show)

class Reduce f where
  reducer :: (a -> b -> b) -> (f a -> b -> b)
  reducel :: (b -> a -> b) -> (b -> f a -> b)

instance Reduce [] where
  reducer (-<) x z = foldr (-<) z x
  reducel (>-) x z = foldl (>-) x z

instance Reduce Node where
  reducer (-<) (Node2 a b) z    = a -< (b -< z)
  reducer (-<) (Node3 a b c) z  = a -< (b -< (c -< z))
  reducel (>-) z (Node2 a b)    = (z >- b) >- a
  reducel (>-) z (Node3 a b c)  = ((z >- c) >- b) >- a

instance Reduce Digit where
  reducer (-<) (One a)          z = a -< z
  reducer (-<) (Two a b)        z = a -< (b -< z)
  reducer (-<) (Three a b c)    z = a -< (b -< (c -< z))
  reducer (-<) (Four a b c d )  z = a -< (b -< (c -< (d -< z)))
  reducel (>-) z (One a)          = z >- a
  reducel (>-) z (Two a b)        = (z >- b) >- a
  reducel (>-) z (Three a b c)    = ((z >- c) >- b) >- a
  reducel (>-) z (Four a b c d)   = (((z >- d) >- c) >- b) >- a

instance Reduce FingerTree where
  reducer (-<) Empty z          = z
  reducer (-<) (Single x) z     = x -< z
  reducer (-<) (Deep pr m sf) z = pr -<< (m -<<< (sf -<< z))
    where (-<<)  = reducer (-<)
          (-<<<) = reducer (reducer (-<))
  reducel (>-) z Empty          = z
  reducel (>-) z (Single x)     = z >- x
  reducel (>-) z (Deep pr m sf) = ((z >>- pr) >>>- m) >>- sf
    where (>>-)  = reducel (>-)
          (>>>-) = reducel (reducel (>-))

toList :: (Reduce f) => f a -> [a]
toList s = s `n` [] where (n) = reducer (:) --reducer (:) s []

toTree :: (Reduce f) => f a -> FingerTree a
toTree s = reducer addL s Empty

addL :: a -> FingerTree a -> FingerTree a
addL v Empty                      = Single v
addL v (Single b)                 = Deep (One v) Empty (One b)
addL v (Deep (One a) m sf)        = Deep (Two v a) m sf
addL v (Deep (Two a b) m sf)      = Deep (Three v a b) m sf
addL v (Deep (Three a b c) m sf)  = Deep (Four v a b c) m sf
addL v (Deep (Four a b c d) m sf) = Deep 
--addL v (Deep [b,c,d,e] m sf)  = Deep [v,b] (addL (Node3 c d e) m) sf
--addL v (Deep pr m sf)         = Deep ([v] ++ pr) m sf

mkFinger = addL 9 $ addL 8 $ addL 7 $ addL 6 $ addL 5 $ addL 4 $ addL 3 $ addL 2 $ addL 1 Empty
