myReturn v = \inp -> [(v, inp)]

myFailure inp = []

myItem inp = case inp of 
			[] -> []
			(x:xs) -> [(x,xs)]

parse p inp= p inp

p +++ q = \inp -> case p inp of 
						[] -> parse q inp
						[(x,xs)] -> [(x,xs)]

sat p = do x <- myItem
    	   if p x then myReturn x else myFailure

--func p = if p == 0 then 5 else 10
char a = sat (==a)

func = do 
	 x <- getChar
	 getChar
	 y <- getChar
	 return (x,y)

foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)

getLine = do 
		x <- getChar;
		if x == '\n' then 
			return []
		else 
			do xs <- Main.getLine
		   	   return (x:xs)

myPutStrLn xs = do putStr xs;
				 putChar '\n'

myPutStr [] = return ()
myPutStr (x:xs) = do putChar x;
					 putStr xs

myStrLen = do putStr "Enter a string: ";
			xs <- Main.getLine;
			putStr "The string has ";
			putStr (show (length xs));
			putStrLn " characters";