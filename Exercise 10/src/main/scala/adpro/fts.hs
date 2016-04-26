data Node a = Node2 a a 
            | Node3 a a a
            deriving (Show)

data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                  deriving (Show)

type Digit a = [a]

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
addL v (Single b)                 = Deep [v] Empty [b]
addL v (Deep [a,b,c,d] m sf)      = Deep [v,a] (addL (Node3 b c d) m) sf
addL v (Deep pr m sf)             = Deep ([v] ++ pr) m sf

addR :: a -> FingerTree a -> FingerTree a
addR v Empty                      = Single v
addR v (Single b)                 = Deep [b] Empty [v]
addR v (Deep pr m [a,b,c,d])      = Deep pr (addL (Node3 b c d) m) [a,v]
addR v (Deep pr m sf)             = Deep pr m (sf ++ [v])

remR :: FingerTree a -> FingerTree a
remR Empty = Empty
remR (Single a) = Empty
remR (Deep pr m [a]) = 
  case m of (Single (Node3 a' b' c')) -> Deep pr (Single (Node2 a' b')) [c']
            (Single (Node2 a' b')) -> Deep pr Empty [a', b']
            (Deep pr' m' [Node3 a' b' c]) -> Deep pr (Deep pr' m' [Node2 a' b']) [c]
            Empty ->  case pr of (x':xs) -> Deep (x' : init xs) m [last xs]
                                 otherwise -> Deep pr m []
            otherwise -> Deep pr m [a] -- It's just a number!
  --in Deep pr m [x]
remR (Deep pr m sf) = Deep pr m (init sf)

remL :: FingerTree a -> FingerTree a
remL Empty = Empty
remL (Single a) = Empty
remL (Deep [_] Empty sf) = Single $ head sf
--remL (Deep [_] )
remL (Deep (_:pr) m sf) = Deep pr m sf

remL' :: Integer -> FingerTree a -> FingerTree a
remL' 0 f = f
remL' n f = remL' (n-1) (remL f)

mkFinger = addL 8 $ addL 7 $ addL 6 $ addL 5 $ addL 4 $ addL 3 $ addL 2 $ addL 1 Empty
mkTree = toTree . reverse . take' [1..] where take' = flip take























