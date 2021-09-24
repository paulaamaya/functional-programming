import Foreign.Marshal (void)

data Expr = 
    -- An integer literal
    NumLiteral Int 
    -- An identifier
    | Identifier String
    -- A function call where
        -- Expr is the function being called
        -- [Expr contains the arguments]
    | Call Expr [Expr] -- A function call

numPlus :: Num a => Expr -> a
numPlus (Call f args) = numPlus f + sum (map numPlus args)
numPlus (Identifier "+") = 1
numPlus (Identifier _) = 0
numPlus (NumLiteral _) = 0