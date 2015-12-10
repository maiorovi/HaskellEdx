data Person = Person {
	firstName :: String,
	lastName :: String,
	age :: Int
} deriving (Show,Eq)

data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow == Yellow = True
	_ == _ = False 

instance Show TrafficLight where
	show Red = "Red light"
	show Yellow = "Yellow light"
	show Green = "Green light"