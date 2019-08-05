import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set  
import System.IO

prefixMod = mod 5 4
infixMode = 5 `mod` 4

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumbers x = if x > 100
                       then x
                       else x * 2

-- doubleSmallNumbers x = if x > 100
--                          then x
--                          else x * 2

zip' :: [a] -> [b] -> [(a, b)]
zip' (a:as) (b:bs) = (a, b) : zip' as bs
zip' _ _ = []

length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b 
    | a < b = b 
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b 
    | a > b  = GT
    | a == b = EQ
    | a < b  = LT

-- initials :: String -> String -> String  
-- initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--  where (f:_) = firstname
--        (l:_) = lastname
    -- where (f:_) = firstname  
          -- (l:_) = lastname

bmi :: (RealFloat a) => a -> a -> a
bmi weight height = weight / (height^2)

-- Q: How to call function A inside function B?
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

multiply' :: (Num a) => [a] -> a -> [a]
multiply' xs x = [ w * x| w <- xs ]

firstIndex elem set = head [i | (i, x) <- zip' [0..] set, x == elem]

replicate' :: (Num a, Ord a) => a -> a -> [a]
replicate' x n 
    | n <= 0 = []
    | otherwise = x: replicate' x (n - 1)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

is100here = elem' 100

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

makeTuple x y = (x, y)

zipWithTuple = zipWith' makeTuple

powerOf2 :: (Integral b) => b -> [Integer]
powerOf2 (-1) = []
powerOf2 n = 2^n: powerOf2 (n - 1)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x: map' f xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n: chain (n `div` 2)
    | otherwise = n: chain (n*3 + 1)

numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

sqrtSums = length' (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- rotateString:: String -> String
-- rotateString str = let (f,l) = splitAt $ (/2) . length' str $ str in l++f
divBy2 n = fromIntegral n / 2

get :: [a] -> Int -> a
get [] _ = error "index too large"
get _ n | n < 0 = error "negative index"
get (x:_) 0 = x
get (_:xs) n = get xs (n - 1)

search:: (Eq a) => [a] -> [a] -> Bool
search part set =
    let nlen = length part
    in foldl (\acc x -> if take nlen x == part then True else acc) False (tails set)

insertAt' :: Int -> b -> [b] -> [b]
insertAt' n _ [] | n < 0 = []
insertAt' 0 e [] = [e]
insertAt' 0 e xs = e: insertAt' (-1) e xs
insertAt' pos e (x:xs) = x: insertAt' (pos - 1) e xs

encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface:: Shape -> Float 
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge:: Shape -> Point -> Shape
nudge (Circle (Point x y) r) (Point a b) = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point a b) = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)  

data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)  

tellCar :: (Show a) => Car String String a -> String  
tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left 
    | x > a = treeElem x right

data TrafficLight = Red | Green | Yellow

class MyEq a where 
    eq :: a -> a -> Bool
    neq :: a -> a -> Bool
    x `eq` y = not (x `neq` y)
    x `neq` y = not (x `eq` y)

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

type Sentence = [Char]

tellTraficLight :: TrafficLight -> Sentence
tellTraficLight Red = "Telling Red"
tellTraficLight Green = "Telling Green"
tellTraficLight Yellow = "Telling Yellow"

any' :: Eq a => [a] -> [a] -> Bool
any' [] _ = False
any' _ [] = False 
any' des src@(_: xs)
    | des == take' (length des) src = True
    | otherwise = any' des xs

data Optional a = Nil | Value a

instance Eq t => Eq (Optional t) where
    Value x == Value y = x == y 
    Nil == Nil = True
    _ == _ = False

add' :: (Num a) => a -> a -> a 
add' = (+)

class MyFunctor t where
    myfmap :: (a -> b) -> t a -> t b

instance MyFunctor [] where
    myfmap = map

instance (Ord k) => MyFunctor (Map.Map k) where
    myfmap = Map.map

multiTwo :: (MyFunctor t, Num b) => t b -> t b
multiTwo = myfmap (*2)

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

instance MyFunctor Tree where
    myfmap f EmptyTree = EmptyTree
    myfmap f (Node x leftLeaf rightLeaf) = Node (f x) (myfmap f leftLeaf) (myfmap f rightLeaf)

class Tofu t where  
    tofu :: j a -> t a j

data Frank a b = Frank {field :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data TypeWrapper a b = TypeWrapper a b deriving (Show)

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance MyFunctor (Barry a b) where
    myfmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
