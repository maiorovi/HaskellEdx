replicateMy n elem | n == 0 = []
				   | otherwise = elem:(replicateMy (n-1) elem)

replicateMy1 n elem = if n == 0 then [] else elem:(replicateMy1 (n-1) elem)