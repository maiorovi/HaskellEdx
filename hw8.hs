getLine' = get []

get xs = do x <- getChar;
			case x of
				'\n' -> return xs
				_ -> get (xs ++ [x])
				

interact' f = do input <- getLine';
				putStrLn (f input)

putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' [] = putChar "\n"
putStrLn' xs = putStr' xs >> putChar '\n'

