{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

--1
fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1) 
--2
fib :: Int -> Int
fib 1 = 1 
fib 0 = 1
fib n = fib (n-1)+ fib(n-2)
--3
myAbs :: Int -> Int
myAbs n
 | n < 0 = -n
 | otherwise = n
--4
composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g = \x -> f (g x)
--5
compose :: (a -> b) -> (c -> a) -> (c -> b)
compose f g = \x -> f (g x)
--6
myConcat :: [a] -> [a] -> [a]
myConcat [] a = a
myConcat (a:b) c = a : myConcat b c
--7
isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (a:c) (b:d)
 |a == b = isIntPrefix c d
 |otherwise = False
--8
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (a:c) (b:d)
 |a == b = isPrefix c d
 |otherwise = False
--9
frepeat :: Int -> (a -> a) -> a -> a
frepeat 1 f x = f x
frepeat n f x = frepeat (n-1) f (f x)
--10

frepeated :: Int -> (a -> a) -> (a -> a)
frepeated 1 f = f
frepeated n f = compose f (frepeated (n-1) f)

main :: IO ()
main = do
    print (fact 10)
    print (fib 10)
    print (myAbs (-10))
    print (myConcat [1, 1, 3] [2, 3, 4])
    print (isIntPrefix [1, 2] [1, 2, 3, 4]) -- True
    print (isIntPrefix [1, 3] [1, 2, 3, 4]) -- False
    print (isIntPrefix [] [1, 2, 3, 4])     -- True
    print (isIntPrefix [1, 2, 3] [1, 2])    -- False
    print (isPrefix [1, 2] [1, 2, 3, 4]) -- True
    print (isPrefix [1, 3] [1, 2, 3, 4]) -- False
    print (isPrefix "abc" "abcdef") -- True
    print (isPrefix "acd" "abcdef") -- False
    print (frepeat 3 (+1) 0) -- 3
    print (frepeat 6 (*2) 1) -- 64
    print ((frepeated 3 (+1))0) --3
    print ((frepeated 6 (*2))1) --3