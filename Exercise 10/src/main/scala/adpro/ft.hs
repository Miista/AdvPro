data Node a = Node2 a a 
            | Node3 a a a
            deriving (Show)

data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
                  deriving (Show)

--data Digit a = One a
--             | Two a a
--             | Three a a a
--             | Four a a a a
--             deriving (Show)
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

(<|) :: (Reduce f) => f a -> FingerTree a -> FingerTree a
(<|) = reducer (addL)
(|>) :: (Reduce f) => FingerTree a -> f a -> FingerTree a
(|>) = reducel (addR')

toList :: (Reduce f) => f a -> [a]
toList s = s `n` [] where (n) = reducer (:) --reducer (:) s []

toTree :: (Reduce f) => f a -> FingerTree a
toTree s = reducer addL s Empty

addL :: a -> FingerTree a -> FingerTree a
addL v Empty                  = Single v
addL v (Single b)             = Deep [v] Empty [b]
addL v (Deep [b,c,d,e] m sf)  = Deep [v,b] (addL (Node3 c d e) m) sf
addL v (Deep pr m sf)         = Deep ([v] ++ pr) m sf

addR' :: FingerTree a -> a -> FingerTree a
addR' b a = addR a b

addR :: a -> FingerTree a -> FingerTree a
addR v Empty                  = Single v
addR v (Single b)             = Deep [b] Empty [v]
addR v (Deep pr m [b,c,d,e])  = Deep pr (addR (Node3 b c d) m) [v,e]
addR v (Deep pr m sf)         = Deep pr m (sf ++ [v])

remL :: FingerTree a -> FingerTree a
remL Empty                        = Empty
remL (Single a)                   = Empty
remL (Deep [_] Empty (a:[])) = Single a
remL (Deep [_] Empty (a:as)) = Deep [a] Empty as
remL (Deep (_:pr) Empty sf)  = Deep pr Empty sf
  --Deep [a] (Single (Node2 b c)) sf
--remL (Deep [_] m sf) = let pr = head (toList m)
--                       in Deep [pr] Empty sf
--remL (Deep [_] (Single (Node3 a b c)) sf) = Deep [a,b,c] Empty sf
--remL (Deep [_] (Single (Node2 a b)) sf) = Deep [a,b] Empty sf
remL a@(Deep [_] m sf) = let ms = tail $ toList a
                             pr' = take 4 ms
                             m' = drop 4 ms
                         in Deep pr' Empty (m')
remL (Deep (_:pr) m sf) = let ms = take 4 $ toList m
                          in Deep [head pr] m sf

headL :: FingerTree a -> a
headL (Single a) = a
headL (Deep pr m sf) = head pr
--headL (Deep [Node3 a _ _] m sf) = a

--remL (Deep (_:[]) Empty [a])         = Single a
--remL (Deep [_] Empty (h:sf))      = Deep [h] Empty sf

--remL f           = toTree . tail $ toList f
--remL (Deep (a:pr) m sf)           = Deep pr m sf

remR :: FingerTree a -> FingerTree a
remR Empty = Empty
remR (Single a) = Empty
--remR (Deep pr m [a,b,c,d]) = Deep pr m [a,b,c]
--remR (Deep pr m [a,b,c]) = Deep pr m [a,b]
--remR (Deep pr m [a,b]) = Deep pr m [a]
--remR (Deep [a,b,c,d] Empty [_]) = Deep [a,b] Empty [c,d]
--remR (Deep [a,b,c] Empty [_]) = Deep [a,b] Empty [c]
--remR (Deep [a,b] Empty [_]) = Deep [a] Empty [b]
--remR (Deep [a] Empty [_]) = Single a
remR (Deep [a] Empty [_]) = Single a
remR (Deep (h:pr) Empty [_]) = Deep (h:(init pr)) Empty [last pr]
remR (Deep pr m (h:sf)) = Deep pr m (h : init sf)
--remR (Deep [a,b,c,d] m sf) = Single as
--remR (Deep [a,b,c,d] m sf) = Single as
--remR (Deep (a) m sf) = Single a

--remR (Deep [a,b,c,d] Empty [_]) = Deep [a,b] Empty [c,d]
--remR (Deep pr (Single n) [_]) = Deep pr Empty (toList n)
--remR (Deep pr m (_:[])) = toTree $ toList m

--transform :: FingerTree a -> FingerTree a

mkFinger = addL 9 $ addL 8 $ addL 7 $ addL 6 $ addL 5 $ addL 4 $ addL 3 $ addL 2 $ addL 1 Empty
mkSimpleFinger = addL 3 $ remR $ remR $ addR 4 $ addR 3 $ addR 1 $ addR 2 Empty
