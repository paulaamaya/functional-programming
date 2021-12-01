
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

lift :: (a -> b) -> (Maybe a -> Maybe b)
lift f Nothing = Nothing 
lift f (Just x) = Just (f x)

safeAdd1ToHead :: [Int] -> Maybe Int
safeAdd1ToHead = lift (+1) . safeHead

safeSecond :: [a] -> Maybe a
safeSecond xs = let xs' = safeTail xs in 
                case xs' of
                    Nothing -> Nothing
                    Just tailElems -> safeHead tailElems 

andThen:: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing 
andThen (Just x) f = f x

safeSecond2 :: [a] -> Maybe a
safeSecond2 xs = safeTail xs `andThen` safeHead

safeFourth :: [a] -> Maybe a
safeFourth xs = safeTail xs `andThen` safeTail `andThen` safeTail `andThen` safeHead

safeDiv2 :: Int -> Int -> Either String Int
safeDiv2 _ 0 = Left "Division by 0"
safeDiv2 x y = Right (div x y)

-- alookup :: AssocList -> String -> Maybe Int 
-- alookup [] s = Nothing 
-- alookup ((k,v):rest) s = if k == s then Just v else alookup rest s