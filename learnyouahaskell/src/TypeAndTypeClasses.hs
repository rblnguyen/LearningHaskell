module TypeAndTypeClasses
    ( removeNonUppercase
    , addThree
    , circumference
    ) where

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c 

-- factorial :: Int -> Int
-- factorial n | n > 10 = product [1..10]
--             | otherwise = product [1..n]

-- factorial' n = if n > 10 then product [1..10] else product [1..n]

fib :: Int -> Int
fib n = if n > 2 then fib (n-1) + fib (n -2) else 1

fib' :: Int -> Int
fib' n 
        | n > 2 = fib (n - 1) + fib (n -2)
        | otherwise = 1
circumference :: Float -> Float
circumference r = 2 * pi * r