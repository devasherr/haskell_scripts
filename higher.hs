-- higher order functions

-- multThree :: Int -> Int -> Int -> Int
multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
-- divideByTen x = x / 10
divideByTen = (/10)

isUppercase :: Char -> Bool
-- isUppercase c = c `elem` ['A'..'Z']
isUppercase = (`elem` ['A'..'Z'])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x == True = x : filter' f xs
    | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = filter' (<=x) xs
        larger = filter' (>x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger 


-- find largest number under 100,000 divisible by 3,829
-- calculates all divisible numbers, reverse the result and gets the head
largestDivisibleBad :: Int
largestDivisibleBad = head (reverse (filter' (f) [1..100000000]))
    where f x = x `mod` 3829 == 0

-- only look for one divisible and when its found just call it a day
largestDivisibleGood :: Int
largestDivisibleGood= head ((filter' (f) [100000000, 99999999..]))
    where f x = x `mod` 3829 == 0

-- For all starting numbers between 1 and 100, how many Collatz chains have a length greater than 15?
collatz :: Integer -> Integer
collatz 1 = 1
collatz x
    | even x = 1+collatz (x `div` 2)
    | otherwise = 1+collatz (x*3 + 1) 

count_collatz :: Integer
count_collatz = sum ([1 | x <- (map collatz [1..100]), x > 15])

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldr (\a acc -> if a == x then True else acc) False

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (\x y -> x * y) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []

and' :: [Bool] -> Bool
-- and' = foldl (&&) True // not lazy -> bad implementation
and' = foldr (&&) True

echo = sum $ takeWhile (<10000) $ filter odd $ map (^2) [1..]