import Data.List
import Data.Char
import Data.Map qualified as Map
import qualified Geometry.Sphere as Sphere

counter' :: String -> [(String, Int)]
counter' = map (\x -> (head x, length x)) . group . sort . words

isSubarray :: (Eq a) => [a] -> [a] -> Bool
isSubarray x = any (isPrefixOf x) . tails

caesar_encode :: String -> Int -> String
caesar_encode s n = map (\c -> chr $ ord c + n) s

caesar_decode:: String -> Int -> String
caesar_decode s n = map (\c -> chr $ ord c - n) s

sumOfDigits :: Int -> Int
sumOfDigits = sum . map digitToInt . show

firstToN :: Int -> (Maybe Int)
firstToN n = find (\x -> sumOfDigits x == n) [1..]

-- dictionary
phoneBook :: Map.Map String String
phoneBook = Map.fromList $ [
    ("bob", "09000000"), 
    ("bob", "09777777"), 
    ("alice", "0911111111"),
    ("kevin", "0922222222")
    ]

-- findKey :: (Eq k) => k -> [(k, v)] -> v
-- findKey key [] = snd . head . filter (\(k, v) -> if k == key then True else False)

-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v):xs)
--     | k == key = Just v
--     | otherwise = findKey key xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

string2Digits :: String -> [Int]
string2Digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add 
    where add x y = x ++ " and " ++ y

-- seems like oop to me :)
echo = Sphere.area 5.6