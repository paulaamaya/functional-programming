-- Some basic function examples
identity :: p -> p
identity x = x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

processNumber :: (Num a, Ord a) => a -> a
processNumber x = doubleSmallNumber x + 1

boomBangList :: Integral a => [a] -> [String]
boomBangList xs = [if x < 5 then "PEW!" else "BANG!" | x <- xs, odd x]

addOne :: Num a => a -> a
addOne x = x + 1

-- Let statement
cylinderArea :: Floating a => a -> a -> a
cylinderArea r h =  let sideArea = 2 * pi * r * h
                        topArea = pi * (r ^ 2)
                    in sideArea + 2 * topArea
