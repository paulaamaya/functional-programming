-- Conditional statement
checker :: Integral a => a -> String
checker x = if even x then "Divisible by 2"
            else if x `mod` 3 == 0 then "Divisible by 2"
            else "I'm too lazy to keep checking"