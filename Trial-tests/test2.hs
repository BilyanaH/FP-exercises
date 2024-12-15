{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

--1a
search :: (Eq a) => a-> [a]->Bool
search _ [] = False
search a (x:xs) 
    |a==x = True
    |otherwise = search a xs

unique :: (Eq a) => [a]->[a]
unique [] = []
unique (x:xs)
    |search x xs = unique xs
    |otherwise = x : unique xs

isNPerm :: Int -> (Int -> Int) -> Bool
isNPerm n f = all (\x -> any (== x) (map f [0 .. n-1])) [0 .. n-1] &&n == length (unique (map f [0 .. n-1]))

--1b

-- Helper function to find a single cycle starting from a given element
findCycleHelper :: (Int -> Int) -> Int -> Int -> [Int] -> [Int]
findCycleHelper f start current visited
    | current `elem` visited = visited
    | otherwise              = findCycleHelper f start (f current) (visited ++ [current])

-- Finds a single cycle starting from the given element
findCycle :: (Int -> Int) -> Int -> [Int]
findCycle f start = findCycleHelper f start start []

-- Finds all disjoint cycles in a permutation
findAllCycles :: Int -> (Int -> Int) -> [[Int]]
findAllCycles n f = findAllCyclesHelper [0..n-1] []
  where
    findAllCyclesHelper [] acc     = acc
    findAllCyclesHelper (x:xs) acc = findAllCyclesHelper xs (acc ++ [findCycle f x])

getMaxhelper::[[Int]]->Int->[Int]
getMaxhelper [] _ = []
getMaxhelper (x:xs) maxLength
    |maxLength == length x = x
    |otherwise = getMaxhelper xs maxLength

getMax::Int -> (Int -> Int)->[Int]
getMax n f =  
    let cycles = findAllCycles n f
        lengthtoSerch = maximum (map length cycles)
    in getMaxhelper cycles lengthtoSerch

maxCycle:: Int-> (Int -> Int) -> [Int]
maxCycle n f
    | not (isNPerm n f) = error "Not a valid n-permutation"
    | otherwise = getMax n f



--2a
movingAverageHelper :: (Fractional a, RealFrac a) => [a]->Int->Int->a->a
movingAverageHelper [] n _ acc = fromIntegral (round ((acc / fromIntegral n)* 10)) / 10
movingAverageHelper _ n 0 acc = fromIntegral (round ((acc/ fromIntegral n)* 10)) / 10
movingAverageHelper (x:xs) n nLeft acc = movingAverageHelper xs n (nLeft-1) (acc+x)

movingAverage ::(Fractional a, RealFrac a) =>  [a] -> Int -> [a]
movingAverage [] _ = []
movingAverage list n = (movingAverageHelper list n n 0) : (movingAverage (tail list) n)

--2b
allAverages ::(Fractional a, RealFrac a) => [a] -> [[a]]
allAverages stream = [movingAverage stream n | n<-[2..]]

--3a
onlyItems::[(String, [String])] -> [String]->[String]
onlyItems _ [] = []
onlyItems inv (x:xs) 
    |elem x (map fst inv) = onlyItems inv xs
    |otherwise            = (onlyItems inv xs)++[x]

allObjectsHelper:: [(String, [String])] -> [(String, [String])] -> [String] -> [String]
allObjectsHelper _ [] acc = acc 
allObjectsHelper inv ((_, items):xs) acc = allObjectsHelper inv xs (acc++(onlyItems inv items))

allObjects :: [(String, [String])] ->[String]
allObjects inv = allObjectsHelper inv inv [] 

--3b
toDelElements ::[String]->[String]->[String]
toDelElements [] items = items
toDelElements (x:xs) items =toDelElements xs  (filter (\y -> y /= x) items )

withOutEmptyBoxes::[String]->[(String, [String])]->[(String, [String])]
withOutEmptyBoxes _ [] = []
withOutEmptyBoxes toDel ((label,items):xs) = [(label, (toDelElements toDel items))] ++ (withOutEmptyBoxes toDel xs)

cleanUpHelper:: [(String, [String])]->[String]-> [(String, [String])]->[(String, [String])]
cleanUpHelper [] toDel acc = withOutEmptyBoxes toDel acc
cleanUpHelper ((label,items):xs) toDel acc 
    |[] == items = cleanUpHelper xs (label:toDel) (withOutEmptyBoxes toDel acc)
    |otherwise  = cleanUpHelper xs toDel (withOutEmptyBoxes toDel acc++[(label,items)])

cleanUp:: [(String, [String])]-> [(String, [String])]
cleanUp inv = cleanUpHelper (cleanUpHelper inv [] []) [] []

main :: IO()
main = do
    print (isNPerm 3 (\x -> (3 - x) `mod` 3)) -- True
    print (isNPerm 10 (`div` 2)) -- False
    print (isNPerm 10 (\x -> (x + 2) `mod` 10)) -- True

    print (maxCycle 3 (\x -> (3 - x) `mod` 3)) -- [1, 2]
    print (maxCycle 10 (\x -> (x + 2) `mod` 10)) -- [0, 2, 4, 6, 8]
    print (maxCycle 10 (\x -> (x + 3) `mod` 10)) -- [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]

    let s = [1076,1356,1918,6252,6766,5525]
    print $ take 4 (movingAverage s 3) -- Резултат: [1450.0,3175.3,4978.6,6181.0, …]
    let s1 = [1076,1356,1918,6252,6766,5525,1000,2000,5000]   
    print $ map (take 10) (take 4 (allAverages s))
    
    let inv = [ ("docs", ["ids", "invoices"]),("ids", ["passport"]),("invoices", []),("memes", []),("family", ["new year", "birthday"]),("funny", ["memes"]),("pics", ["family", "funny"]) ]
    print $ allObjects inv
    print $ cleanUp inv

