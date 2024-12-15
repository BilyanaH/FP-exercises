{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

--1
len:: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

--2
exists :: (a->Bool) -> [a] -> Bool
exists p = foldr (\x b -> p x || b) False

--3
myForall :: (a->Bool) -> [a] -> Bool
myForall p = foldr (\x b -> p x && b) True

--4
--eq - za da znaem che tipa moje da se srawnqwa
member :: Eq a => a -> [a] -> Bool
member x = exists (\y -> x == y)

--5
listMap :: (a->a) -> [a] -> [a]
listMap _ [] = []
listMap f (x:xs) = [f x]++(listMap f xs)

--6
listFilter :: (a->Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter p (x:xs)
 | p x = [x]++listFilter p xs
 | otherwise = listFilter p xs

 --7
push :: a -> [a] -> [a]
push x l = l ++ [x]

 --8 
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

--9
insert :: a -> Int -> [a] -> [a]
insert x 0 l = [x]++l
insert x _ [] = [x]
insert x n (y:ys) = y:insert x (n - 1) ys

--10
append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x:xs) l2 = x : append xs l2

--11
listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr _ acc [] = acc
listFoldr op acc (x:xs) = op x (listFoldr op acc xs)

--12
listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl _ acc [] = acc
listFoldl op acc (x:xs) = listFoldl op (op acc x) xs

--13
mySum :: Num a => [a] -> a
mySum = (listFoldr (+) 0 )

--14
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (listFilter (<= x) xs) ++[x]++ quickSort (listFilter (> x) xs)

main :: IO()
main = do
    putStrLn "Testing len function:"
    print $ len [1, 2, 3]        -- Expected output: 3
    print $ len ([] :: [Int])    -- Expected output: 0

    putStrLn "\nTesting exists function:"
    print $ exists even [1, 3, 5]   -- Expected output: False
    print $ exists odd [1, 3, 5, 6]    -- Expected output: True

    putStrLn "\nTesting forall function:"
    print $ myForall even [2, 4, 6]   -- Expected output: True
    print $ myForall odd [1, 3, 5]    -- Expected output: True
    print $ myForall odd [1, 2, 3]    -- Expected output: False

    putStrLn "\nTesting member function:"
    print $ member 3 [1, 2, 3, 4]   -- Expected output: True
    print $ member 5 [1, 2, 3, 4]   -- Expected output: False

    putStrLn "\nTesting listMap function:"
    print $ listMap (\x -> (x + 2)) [1, 2, 3, 4]   -- Expected output: [3, 4, 5, 6]
    print $ listMap (+ 3) [1, 2, 3, 4]   -- Expected output: [4,5,6,7] 

    putStrLn "\nTesting listFilter function:"
    print $ listFilter even [1, 2, 3, 4, 5]   -- Expected output: [2, 4]
    print $ listFilter odd [1, 2, 3, 4, 5]    -- Expected output: [1, 3, 5]
    print $ listFilter (> 3) [1, 2, 3, 4, 5]  -- Expected output: [4, 5]
    print $ listFilter (< 0) [1, 2, 3, 4, 5]  -- Expected output: []

    putStrLn "\nTesting push function:"
    print $ push 5 [1, 2, 3, 4]    -- Expected output: [1, 2, 3, 4, 5]
    print $ push 'a' "hello"       -- Expected output: "helloa"
    print $ push 10 []             -- Expected output: [10]
    print $ push True [False]      -- Expected output: [False, True]

    putStrLn "\nTesting reverse function:"
    print $ myReverse [1, 2, 3, 4, 5]    -- Expected output: [5, 4, 3, 2, 1]
    print $ myReverse "hello"           -- Expected output: "olleh"
    print $ myReverse [True, False]     -- Expected output: [False, True]
    print $ myReverse ([] :: [Int])     -- Expected output: []

    putStrLn "\nTesting insert function:"
    print $ insert 10 0 [1, 2, 3, 4]  -- Expected: [10, 1, 2, 3, 4]
    print $ insert 10 2 [1, 2, 3, 4]  -- Expected: [1, 2, 10, 3, 4]
    print $ insert 10 4 [1, 2, 3, 4]  -- Expected: [1, 2, 3, 4, 10]
    print $ insert 10 0 []           -- Expected: [10]
    print $ insert 10 2 []           -- Expected: [10] (inserted at the end of an empty list)

    putStrLn "\nTesting append function:"
    print $ append [1, 2, 3] [4, 5, 6]  -- Expected: [1, 2, 3, 4, 5, 6]
    print $ append [] [4, 5, 6]         -- Expected: [4, 5, 6]
    print $ append [1, 2, 3] []         -- Expected: [1, 2, 3]
    print $ append ([] :: [Int]) ([] :: [Int])   -- Expected: []

    putStrLn "\nTesting listFoldr:"
    print $ listFoldr (\x xs -> x:xs) [] [1, 2, 3]      -- Expected: [1, 2, 3]
    print $ listFoldr (+) 0 [1, 2, 3, 4]    -- Expected: 10
    print $ listFoldr (*) 1 [1, 2, 3, 4]    -- Expected: 24
    print $ listFoldr (-) 0 [1, 2, 3, 4]    -- Expected: -2 (1 - (2 - (3 - (4 - 0))))

    putStrLn "\nTesting listFoldl:"
    print $ listFoldl (\xs x -> x : xs) [] [1, 2, 3] -- Expected: [3, 2, 1]
    print $ listFoldl (+) 0 [1, 2, 3, 4]    -- Expected: 10
    print $ listFoldl (*) 1 [1, 2, 3, 4]    -- Expected: 24
    print $ listFoldl (-) 0 [1, 2, 3, 4]    -- Expected: -10 (((0 - 1) - 2) - 3) - 4

    putStrLn "\nTesting sum with listFoldr:"
    print $ mySum [1, 2, 3, 4]                -- Expected: 10
    print $ mySum []                          -- Expected: 0

    putStrLn "\nTesting quickSort function:"
    print $ quickSort [6, 0, 3, -1, 5]  -- Expected output: [-1,0,3,5,6]
    print $ quickSort "hello"           -- Expected output: "ehllo"
    print $ quickSort [True, False]     -- Expected output: [False, True]
    print $ quickSort ([] :: [Int])     -- Expected output: []

