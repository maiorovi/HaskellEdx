type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m*n

copy :: a -> Pair a
copy x = (x,x)


data Shape = Circle Float | Rect Float Float deriving (Show)

square n = Rect n n


area (Circle r) =  pi * r * r

area (Rect x y) = x * y