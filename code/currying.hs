bigList :: [Integer] -> [Integer]
bigList xs = filter ((<) 5) (xs)