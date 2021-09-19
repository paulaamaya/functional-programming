factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial(n - 1)


-- Value based conditional
foo :: (Eq p, Num p) => p -> p
foo x = if x == 5
    then 10
    else if x == 10
        then 15
    else x + 13

-- Value pattern matching
foo2 :: (Eq p, Num p) => p -> p
foo2 5 = 10
foo2 10 = 15
foo2 x = x + 13

-- Strucuture based conditional
listMax :: (Fractional a, Ord a) => [a] -> a
listMax lst = if null lst then - 1 / 0
            else max (head lst) (listMax (tail lst))

-- Structural pattern matching
listMax2 [] = -1/0
listMax2 (x:xs) = max x (listMax2 xs)

-- Expression based if statements
checker :: Integral a => a -> String
checker x = if even x then "Divisible by 2"
            else if x `mod` 3 == 0 then "Divisible by 3"
            else "I'm too lazy to keep checking"

-- Guards
