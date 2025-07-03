triplets = [[a, b, c] | c <- [1..10], a <- [1..c], b <- [1..a], a^2+b^2==c^2]
target x' = [x | x <- x', sum x == 24]