import qualified Data.Map as Map
-- custom types
data Point = Point Float Float deriving(Show)
data Shape = Circle Point Float | Rectangle Point Point deriving(Show)

-- data Person = Person {
--     firstName :: String,
--     lastName :: String,
--     age :: Int
-- } deriving(Show, Eq)

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
} deriving(Show)

-- parameterized type
data Car c m y = Car {
    company :: c,
    model :: m,
    year :: y
} deriving(Show)

tellCar :: Car String String Int -> String
tellCar (Car {company=c, model=m, year=y}) = "The" ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving(Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector(i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i+j + j*m + k*n

vmul :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmul` m = Vector (i*m) (j*m) (k*m)

data Day = Monday | Tuesday | Wednesday | Sunday deriving(Show, Eq, Ord, Bounded, Enum)

data LockerState = Taken | Free deriving(Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker doesn't exist"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken"
    
-- recursive data structures

infixr 5 :-:
-- data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving(Show, Read, Eq, Ord)
data List a = Empty | a :-: (List a) deriving(Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ (ys) = x :-: (xs ^++ ys)

-- Binary Search Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- insert into tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- check is node exists in tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red Light"
    show Green = "Green Light"
    show Yellow = "Yellow Light"

-- lets implement class YesNo a where
    yesno :: a -> Bool
instance 