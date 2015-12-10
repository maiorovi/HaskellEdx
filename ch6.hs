--import Prelude hiding (filter, map)

twice f x = f (f x)
mapMy f [] = []
mapMy f (x:xs) = f x : map f xs

filterMy p [] = []
filterMy p (x:xs) | p x = x:filter p xs
				  | otherwise = filter p xs

foldrMy f v [] = v
foldrMy f v (x:xs) = f x (foldrMy f v xs)

oddMy = not . even

-- not :: (Bool->Bool) 
-- even (Int->Bool)
takeWhileMy p [] = []
takeWhileMy p (x:xs)
				| p x = x:takeWhile p xs
				| otherwise = []
allMy p [] = True
allMy p (x:xs) 
		| p x = allMy p xs
		| otherwise = False

--allMy1 p (x:xs) = foldrMy (&&) True (x:xs)

dropWhileMy p [] = []
dropWhileMy p (x:xs) 
			| p x = dropWhile p xs
			| otherwise = (x:xs)