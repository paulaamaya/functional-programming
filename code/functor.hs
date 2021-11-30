
data List a = Empty | Cons a (List a) deriving Show

instance Functor List where
    -- fmap:: (a -> b) -> List a -> List b
    fmap f Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data Pair a = Pair a a deriving Show

instance Functor Pair where
    -- fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)


data MyMaybe a  = MyNothing | MyJust a deriving Show

instance Functor MyMaybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)