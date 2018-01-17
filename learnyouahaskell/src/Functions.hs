module Functions
( lucky
, lucky'
, sayMe
, factorial
, factorial'
, factorial3
, factorial31
, factorial32
, divWithTry
) where

import Control.Exception

someFunc1 :: IO ()
someFunc1 = putStrLn "someFunc1"


lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven"
lucky _ = "Better luck next time"

lucky' :: (Integral a) => a ->String
lucky' n = if n == 7 then "Lucky number 7" else "Better luck next time"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe _ = "Not Between 1 and 5"

factorial :: Int -> Int
factorial n | n > 10 = product [1..10]
            | otherwise = product [1..n]

factorial' n = if n > 10 then product [1..10] else product [1..n]

factorial3 :: (Integral a) => a -> a
factorial3 n = if n == 0 then 1 else n * factorial3(n - 1)

factorial31 :: (Integral a) => a -> a
factorial31 n | n == 0 = 1 
                |otherwise =  n * factorial3(n - 1)

factorial32 :: (Integral a) => a -> a
factorial32 0 = 1
factorial32 n = n * factorial3(n - 1)

fib :: Int -> Int
fib n = if n > 2 then fib (n-1) + fib (n -2) else 1

fib' :: Int -> Int
fib' n 
        | n > 2 = fib (n - 1) + fib (n -2)
        | otherwise = 1

addVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c ) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "The list is empty. Can't call head"
head' (x:_) = x

first3 :: [a] -> [a]
first3 [] = error "The list is empty"
first3 (x:xs) = error "list only have one item"

divWithTry n = do
        let x = 5 `div` n
        result <- try (evaluate (x)) :: IO (Either IOException ())
        case result of
                Left  _ -> return n
                Right () -> return n