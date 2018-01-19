module HighOrderFunctions
( highOrderFunctions
, zipWith'
, map'
, filter'
, largestDivisible
, sumOfOdds





) where

highOrderFunctions :: IO ()
highOrderFunctions = putStrLn "HighOrderFunctions"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
    | f x = x : filter' f xs
    | otherwise = filter' f xs

quicksort2 [] = []
quicksort2 (x:xs) = 
    let smallList = quicksort2 (filter' (<= x) xs)
        bigList = quicksort2 (filter' (> x) xs)
    in smallList ++ [x] ++ bigList

largestDivisible = head $ filter p [100000, 99999..]
  where p x = x `mod` 3890 == 0

sumOfOdds :: (Integral s) => [s] -> s
sumOfOdds [] = 0
sumOfOdds list = sum (takeWhile (<10000) (filter odd (map (^2) list)))

-- zipWith, map, fmap, filter, takeWhile, odd, even, fromIntegral, !! get elem at index , 