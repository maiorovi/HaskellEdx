import Data.Char

divides x y | x `mod` y == 0 = True
			| otherwise 	 = False

divisors x = [z | z <- [1..x], divides x z]

rifle xs ys = concat [[x,y]| (x,y) <- zip xs ys]

factors x = [y | y <- [1..e], x `mod` y == 0]
			where e = x - 1

let2int c = ord c - ord 'a'

int2let	n = chr (ord 'a' + n)

let2IntU c = ord c - ord 'A'
int2letU n = chr (ord 'A' + n)

shift n c | isLower c = int2let((let2int c + n) `mod` 26)
		  | isUpper c = int2letU((let2IntU c + n) `mod` 26) 
		  | otherwise  = c

encode n xs = [shift n x | x <- xs]