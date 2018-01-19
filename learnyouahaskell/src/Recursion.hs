module Recursion
( recursionFunc
, fib
, fib'
, maximum1
, maximum2
, maximum3
, replicate1
, replicate2
, take1
, reverse1
, zip2List
, elem1
, quicksort
) where

recursionFunc :: IO ()
recursionFunc = putStrLn "RecursionFunc"


fib :: Int -> Int
fib n = if n > 2 then fib (n-1) + fib (n -2) else 1

fib' :: Int -> Int
fib' n 
        | n > 2 = fib (n - 1) + fib (n -2)
        | otherwise = 1

maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "No max. This is an empty list"
maximum1 [x] = x
maximum1 (x:xs) = do 
    let maxValue = maximum1 xs
    if x > maxValue 
    then x
    else  maxValue 

maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "No max. This is an empty list"
maximum2 [x] = x
maximum2 (x:xs) 
    | x > maxValue = x
    | otherwise = maxValue
    where maxValue = maximum2 xs

maximum3 :: (Ord a) => [a] -> a
maximum3 [] = error "No max. This is an empty list"
maximum3 [x] = x
maximum3 (x:xs) = max x $ maximum3 xs

replicate1 :: (Ord t, Num t) => a -> t -> [a]
replicate1 n x = if x <= 0 then [] else n : replicate1 n (x - 1)

replicate2 :: (Ord t, Num t) => a -> t -> [a]
replicate2 n x 
    | x <= 0 = []
    | otherwise = n : replicate2 n (x -1)

take1 :: (Num i, Ord i) => i -> [a] -> [a]
take1 n _
    | n <= 0 = []
take1 _ [] = []
take1 n (x:xs) = x : take1 (n - 1) xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 [x] = [x]
reverse1 (x:xs)  =  reverse1 xs ++ [x]

zip2List :: [a] -> [b] -> [(a,b)]
zip2List _ [] = []
zip2List [] _ = []
zip2List (x:xs) (y:ys) = (x,y) : zip2List xs ys

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs)
    | a == x = True
    | otherwise = a `elem1` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let bigList = quicksort [a | a <- xs, a > x]
        smallList = quicksort [b | b <- xs, b <= x]
    in smallList ++ [x] ++ bigList