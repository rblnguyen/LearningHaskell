module HighOrderFunctions
where

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

-- LAMBDA
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChain :: Int
numLongChain = length (filter' isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numLongChain' :: Int
numLongChain' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- foldl, foldr, foldl1 and foldr1
sumByFoldl :: (Num a) => [a] -> a
sumByFoldl = foldl (+) 0

sumByFoldl1 xs= foldl1 (+) xs

elemByFoldl a ys = foldl (\acc y ->if a == y then True else acc) False ys 
elemByFoldl1 a ys = foldl1 (\acc y -> if a == y then True else acc) ys

-- ++ add two LIST; that's why we wrap f x inside [f x]
mapByFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs
mapByFoldl1 f xs = foldl1 (\acc x -> acc ++ [f x]) xs

-- prepend (add at the begining vs append: add at the end)
-- : prepend a VALUE to a list. 
mapByFoldr f xs = foldr (\x acc -> f x : acc) [] xs

-- foldr1 and foldl1 work similar to foldr and foldl but doesn't need the starting value
-- however, they expect the list to have at least 1 value or it will throw an exception at runtime
mapByFoldr1 f xs = foldr1 (\x acc -> f x : acc) xs

