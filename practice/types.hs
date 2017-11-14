data Point = Point Float Float deriving (Show)
data Vector = Vector Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs (y2 - y1))

transform :: Shape -> Vector -> Shape
transform (Circle (Point x1 y1) r) (Vector dx dy) = Circle (Point (x1 + dx) (y1 + dy)) r
