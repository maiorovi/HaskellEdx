concat1 xss = [x | xs <- xss, x <- xs]

factor n = [x | x <- [1..n], n `mod` x == 0]

prime n = factor n == [1,n]

primes n = [x | x <- [2..n], prime n]

pairs xs = zip xs (tail xs)

sorted xs = and [x <= y | (x,y) <- pairs xs]

zipWithPositions xs = [ (y,x) | (x,y) <- zip xs [0..n]]
				where n = length xs - 1

lowers xs = [x | x <- xs, isLower x]