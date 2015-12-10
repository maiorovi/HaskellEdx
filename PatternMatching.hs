lucky 7 = "LUCKY NUMBER SEVEN!"
lucky n = "Sorry, you are out of luck!"
--halve :: [a] -> ([a],[a])
--halve (xs) | even (length xs) = (take (length xs / 2), drop (length xs / 2))
--		   | otherwise = ([],[])
safetail (x:xs) = xs
safetail [] = []


safetailG xs | length xs /= 0 = tail xs
			 | otherwise = []

safetailCond xs = if (length xs /= 0) then tail xs else []

myOr False False = False
myOr _ _ = True

myAnd a b = if (a && b) then True else False

conjunction a b = if (a == True) then b else False

concatMy xss = [x | xs <- xss, x<-xs]

firstMy ps = [x | (x, _)  <- ps]

-- guards 
evens xs = [x | x <- xs, even x]

safetail2 xs | null xs = []
			| otherwise =  tail xs

safetail3 
	= \ xs ->
		case xs of
			[] -> []
			(_ : xs) -> xs