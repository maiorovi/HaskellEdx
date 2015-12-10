import Data.Char

type Parser a = String -> [(a, String)]

parse p inp = p inp

p +++ q = \inp -> case parse p inp of
						[] -> parse q inp
						[(v,out)] -> [(v.out)]

myFilter p = foldr (\ x xs -> if p x then x:xs else xs) []

myCurry f = \ x y -> f (x,y)

myF (x,y) = x + y

myUncurry f = \ (x,y) -> f x 

myAsll p = foldr (&&) True . map p

myAny p xs = foldr (||) True (map p xs)

chop8 = unfold null (take 8) (drop 8)
unfold p h t x
			| p x = []
			| otherwise = h x : unfold p h t (t x)

myMap f = unfold null (f (head)) tail

myIterate f = unfold (const False) f f

mCompose f g x = f (g x)

test xs ys = reverse ys ++  reverse xs

return v = \inp -> [(v, inp)]

failure = \inp -> []

item = \inp -> case inp of 
				[] -> []
				(x:xs) -> [(x,xs)]
isDigit' = \inp -> isDigit (fst (inp!!0))

sat p = do x <- item;
		 if p x then
		 	Main.return x
		 else
		 	Main.failure

char x = sat (x ==)

--many p = many1 p +++ Main.return []

--many1 p = do v <- p;
--			 vs <- many p;
--			 Main.return (v:vs)