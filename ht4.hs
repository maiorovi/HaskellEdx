--halve xs = (take n xs, drop n xs)
--		where n = length xs / 2

halve xs = splitAt (length xs `div` 2) xs