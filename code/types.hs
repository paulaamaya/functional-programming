data Point = Point Float Float

distance :: Point -> Point -> Float 
distance (Point x1 y1) (Point x2 y2) = 
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
    in 
        sqrt (dx*dx + dy*dy)

makePoints :: [Float] -> [Float] -> [Point]
makePoints = zipWith Point

data Shape = Circle Point Float
            | Rectangle Point Point

area :: Shape -> Float 
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs ((x1 - x2) * (y1 - y2))

-- make Point a member of typeclass Show
instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"