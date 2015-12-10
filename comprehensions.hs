pairs xs = zip xs (tail xs)

isSorted xs = and [x <= y | (x,y) <- pairs xs]