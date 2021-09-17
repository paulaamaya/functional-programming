identity :: p -> p
identity x = x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

processNumber :: (Num a, Ord a) => a -> a
processNumber x = doubleSmallNumber x + 1

boomBangList :: Integral a => [a] -> [String]
boomBangList xs = [if x < 5 then "PEW!" else "BANG!" | x <- xs, odd x]

matrix = [(1,2), (3,4), (1,1)]

addOne :: Num a => a -> a
addOne x = x + 1
