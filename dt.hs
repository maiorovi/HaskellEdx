data Person = Person { firstName :: String,
					   lastName :: String,
					   age :: Int,
					   height:: Float,
					   phoneNumber :: String,
					   flavour::String
					 } deriving(Show)


data Car a b c = Car { company :: a,
					   model :: b,
					   year :: c		
					 } deriving (Show)

tellCar (Car {company = c, model = m, year=y}) = "This " ++ c ++ " " ++ m ++ " was made " ++ show y


data Vector a = Vector a a a deriving (Show)

(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

(Vector i j k) `vmult` m = Vector (i*m) (j*m) (m*k)

(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n