{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

--1
nats::[Integer]
nats = [0..]

--2
isPrime:: Integer -> Bool
isPrime n = n>1 && all (\d ->mod n d /= 0) [2..(n-1)]

primes::[Integer]
primes = filter isPrime nats

--3
primes2::[Integer]
primes2 = sieve [2..]
    where 
        sieve [] = [] 
        sieve (x:xs) = x : sieve [y|y<-xs, mod y x /=0]

--4
myIterate :: (a->a)->a->[a]
myIterate f x = x: (myIterate f (f x))

--5
rats :: [(Integer, Integer)]
rats = [(a, b) | a <- [1..], b <- [-a..a], gcd a b == 1] 

--6
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--7
data BinTree a
    = Node a (BinTree a) (BinTree a)
    | Empty

instance Show a => Show (BinTree a) where
    show tree = unlines (showTree tree)
      where
        showTree Empty = []
        showTree (Node value left right) =
            [show value] ++ shift "├── " "│   " (showTree left) ++ shift "└── " "    " (showTree right)
        shift first other = zipWith (++) (first : repeat other)

--8
trimBinTree :: Integer -> (BinTree a) -> (BinTree a)
trimBinTree 0 _ = Empty
trimBinTree _ Empty = Empty
trimBinTree n (Node value left right) =  Node value (trimBinTree (n - 1) left) (trimBinTree (n - 1) right)

--9
babaTree :: BinTree String
babaTree = generate ""
    where  
        generate value = Node value (generate (value++"a")) (generate (value++"b" ))

main :: IO ()
main = do
    putStrLn "Firt 50 nats:"
    print $ take 50 nats

    putStrLn "Firt 50 primes:"
    print $ take 50 primes
    print $ take 50 primes2

    putStrLn "First 30 powers of 2 via iterate:"
    print $ take 30 (myIterate (* 2) 1)

    putStrLn "First 100 rational numbers:"
    print $ take 100 rats

    putStrLn "First 100 fibs:"
    print $ take 100 fibs

    putStrLn "Cut babaTree tree to depth 6:"
    print $ trimBinTree 6 babaTree