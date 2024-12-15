{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
--1
data Shape
    = Circle Float
    | Rectangle Float Float
    | Ngon Int Float
    deriving (Show)

area :: Shape -> Float
area (Rectangle a b) = a * b
area (Circle a) = a * a * 3.14
--area (Ngon n s) = (fromIntegral n * s^2) / (4 * tan (pi / fromIntegral n))
area (Ngon n s) = (fromIntegral  n )* (s/2) * (s/2) * tan  ((180 / fromIntegral n) * (pi / 180))
--2
perimeter :: Shape -> Float
perimeter (Rectangle a b) = 2*(a + b)
perimeter (Circle a) = a * 2 * 3.14
perimeter (Ngon n s) = (fromIntegral  n )* s* tan  ((180 / fromIntegral n) * (pi / 180))
--3
data RPS
    = Rock
    | Paper
    | Scissors
    deriving (Show)

beats :: RPS -> RPS -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False
--4
data List a
    = Cons a (List a)
    | Empty
   -- deriving (Show)

lmap :: (a -> b) -> List a -> List b
lmap _ Empty = Empty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)
--5
lfilter :: (a-> Bool) -> List a -> List a
lfilter _ Empty = Empty
lfilter f (Cons x xs)
    | f x = Cons x (lfilter f xs)
    |otherwise = lfilter f xs
--6
lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr _ acc Empty = acc
lfoldr f acc (Cons x xs) = f x (lfoldr f acc  xs)
--7
class Countable a where
    count :: a -> Int

instance Countable (List a) where
    count (Cons _ l) = (count l) + 1
    count Empty      = 0

class Averagable a where 
    avg :: a -> Float

instance Averagable (List Float) where 
    avg Empty = 0
    avg l = sum' l / fromIntegral (count l)
        where
            sum' :: List Float -> Float
            sum' Empty = 0
            sum' (Cons x xs) = x + sum' xs

--8 
instance (Show a) => Show (List a) where
    show Empty = "List()"
    show l = "List(" ++ showElems l ++ ")"
        where
            showElems Empty = ""
            showElems (Cons y Empty) = show y
            showElems (Cons y ys) = show y ++ " " ++ showElems ys
        
--9
data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)
    deriving (Show)

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left right) =
    abs (height left - height right) <= 1 && isBalanced left && isBalanced right
  where
    height :: Tree a -> Int
    height Leaf = 0
    height (Node _ l r) = 1 + max (height l) (height r)
    
main :: IO ()
main = do
    -- Проверка на area и perimeter
    let shapes = [Circle 5.0, Rectangle 3.0 4.0, Ngon 6 2.0]
    putStrLn "Shapes and their areas:"
    mapM_ (\s -> putStrLn $ show s ++ " -> area: " ++ show (area s)) shapes
       
    putStrLn "\nShapes and their perimeters:"
    mapM_ (\s -> putStrLn $ show s ++ " -> perimeter: " ++ show (perimeter s)) shapes
   
   -- Проверка на beats
    putStrLn "\nRock-Paper-Scissors results:"
    let rpsMoves = [(Rock, Scissors), (Scissors, Paper), (Paper, Rock), (Rock, Rock)]
    mapM_ (\(x, y) -> putStrLn $ show x ++ " vs " ++ show y ++ ": " ++ show (beats x y)) rpsMoves

    -- Проверка на List функции
    let lst = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
    putStrLn "\nList operations:"
    putStrLn $ "Original list: " ++ show lst
    putStrLn $ "Mapped (+1): " ++ show (lmap (+1) lst)
    putStrLn $ "Filtered (>2): " ++ show (lfilter (>2) lst)
    putStrLn $ "Folded to sum: " ++ show (lfoldr (+) 0 lst)

    -- Проверка на Countable и Averageable
    let floatList :: List Float
        floatList = Cons 1.5 (Cons 2.5 (Cons 3.5 Empty))    
    putStrLn "\nAverageable:"
    putStrLn $ "Float list: " ++ show floatList
    putStrLn $ "Average: " ++ show (avg floatList)

    -- Проверка на двоичното дърво
    let balancedTree = Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)
    let unbalancedTree = Node 5 (Node 3 (Node 1 Leaf Leaf) Leaf) Leaf
    putStrLn "\nTree balance checks:"
    putStrLn $ "Balanced tree: " ++ show balancedTree ++ " -> " ++ show (isBalanced balancedTree)
    putStrLn $ "Unbalanced tree: " ++ show unbalancedTree ++ " -> " ++ show (isBalanced unbalancedTree)