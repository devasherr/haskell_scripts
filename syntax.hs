addVector :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third(_, _, x) =  x

-- echo :: [Int]
-- echo = [a+b | (a, b) <- [(1, 2), (3, 4), (5, 6)]]

-- head' :: [a] -> a
-- head' [] = error "can't call head on an empty list"
-- head' (x:_) = x

head' :: [a] -> a
head' xs = case xs of [] -> error "can't call head on an empty list!"
                      (x:_) -> x


tell :: (Show a) => [a] -> String
tell [] = "the list is empty"
tell ([x]) = "the list has one element: " ++ show x
tell ([x, y]) = "the list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is too long"

firstLetter :: String -> String
firstLetter "" = "string is empty"
firstLetter all@(x:xs)= "first letter of " ++ all ++ " is " ++ [x]

max' :: Ord(a) => a -> a -> a
max' x y
    | x >= y = x
    | otherwise = y

bmiTell :: Double -> Double -> String
bmiTell w h
    | bmi <= 18.5 = "your underweight"
    | bmi <= 25.0 = "your normal"
    | bmi <= 30.0 = "your fat"
    | otherwise = "your very very fat"
    where bmi  = w / (h ^ 2)

badGreeting :: String
badGreeting = "ohh, its just you"

goodGreeting :: String
goodGreeting = "hey, haven't seen you in a while"

sayHello :: String -> String
sayHello "yafet" = goodGreeting
sayHello "babi" = goodGreeting
sayHello name = badGreeting ++ name

echo = [let square x = x ^ 2 in (square 5, square 3)]

cylinder :: Double -> Double -> Double
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]