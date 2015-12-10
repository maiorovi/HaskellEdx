import System.IO;

hangman = do putStrLn "Think of a word:";
		  word <- sgetLine;
		  putStrLn "Try to guess it!";
		  guess word

sgetLine = do x <- getCh;
			if x == '\n' then
				do putChar x;
					return [];
			else
				do putChar '-';
					xs <- sgetLine;
					return (x:xs)

getCh = do hSetEcho stdin False;
			c <- getChar;
			hSetEcho stdin True;
			return c;


guess word = do putStr "> ";
				xs <- getLine;
				if xs == word then
					putStrLn "You got it"
				else
					do putStrLn (diff word xs)
					   guess word

diff xs ys = [if elem x ys then x else '-' | x <- xs]