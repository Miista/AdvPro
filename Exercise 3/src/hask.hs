data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

sizeT :: Tree a -> Int
sizeT (Leaf _) = 1
sizeT (Branch l r) = (sizeT l) + (sizeT r) + 1

mkTree = Branch (Leaf 1) (Branch (Leaf 1) (Leaf 2))

data Option a = Some a
    | None
    deriving (Show)

mapO :: Option a -> (a -> b) -> Option b
mapO (Some a) f = Some (f a)
mapO None _ = None

 --b -> (a -> b) -> Maybe a -> b
getO :: Option a -> (a -> b) -> b -> b
getO (Some a) f _ = f a
getO None f b = b