-- Recursive construction of a list
l :: [Integer]
l = [3,2,1]
x :: Integer
x = 4
y :: Integer
y =5

xs :: [Integer]
xs = x : l
-- [4,3,2,1]

ys :: [Integer]
ys = y : xs
-- [5,4,3,2,1]

-- Operations on lists

{- Write a function that calculates the sum of elements in the list
>>> sumList [1,32,3,17,11]
64
-}
sumList :: [Integer] -> Integer
sumList xs = if null xs
                then 0
            else head xs + sumList (tail xs)

{- Write a function that finds the largest element in a list
>>> maxList [1,32,3,17,11]
32.0
-}
maxList :: (Fractional a, Ord a) => [a] -> a
maxList xs = if null xs
                then - 1/ 0
            else max (head xs) (maxList (tail xs))
