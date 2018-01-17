module Functions
( lucky
, lucky'
, sayMe
, factorial
, factorial'
, factorial3
, factorial31
, factorial32
, tell
, length'
, sum'
, capital
, divWithTry
, bmiTell
, getBMIs
, getBMIs'
, getInitial
) where

import Control.Exception

someFunc1 :: IO ()
someFunc1 = putStrLn "someFunc1"

-- PATTERN MATCHINGS ----
-------------------------
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

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell [x] = "Only one element: " ++ show x
tell [x, y] = "Two elements: " ++ show x ++ ", " ++ show y 
tell [x, y, z]= "Three elements: "++ show x ++ ", " ++ show y ++ ", " ++ show z
tell (x:y:z:_) = "Long list. Here are the first 3: "++ show x ++ ", " ++ show y ++ ", " ++ show z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num p) => [p] -> p
sum' [] = 0
sum' (x:xs) = x + sum' xs

--capital [] = "Empty String"
capital "" = "Empty String"
capital all @ (x:ys) = "First letter of " ++ all ++ " is " ++ [x]



divWithTry n = do
        let x = 5 `div` n
        result <- try (print x) :: IO (Either IOException ())
        case result of
                Left  _ -> putStrLn "Error"
                Right () -> putStrLn "OK"

-- GUARD: Testing a value's properties are true/false ----
----------------------------------------------------------
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height  -- weight: kg; heigh: meter
    | bmi' weight height <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise = "You're a whale, congratulations!" 
    where bmi = weight / (height ^ 2)
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
          (skinny1, normal1, fat1) = (18.5, 25.5, 30.0)


bmiTell' bmi  -- weight: kg; heigh: meter
        | bmi <= skinny = "underweight"  
        | bmi <= normal = "normal"  
        | bmi <= fat = "fat"  
        | otherwise = "You're a whale, congratulations!" 
        where   skinny = 18.5  
                normal = 25.0  
                fat = 30.0  
                (skinny1, normal1, fat1) = (18.5, 25.5, 30.0)

bmi':: (RealFloat b) => b -> b -> b
bmi' w h = w / (h^2)

getBMIs xs = [bmiTell' $ bmi w h | (w,h) <- xs ]
        where bmi weight height = weight / (height ^ 2)

getInitial :: String -> String -> String
getInitial first lastname = [f] ++ ". " ++ [l] ++ "."  
        where 
                f:_ = first
                l:_ = lastname

-- LET BINDING
--cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
        let sideArea = 2 * pi * r * h
            topArea = pi * r ^ 2
        in sideArea + 2 * topArea

getBMIs' xs = [bmiTell' bmi | (w,h) <- xs, let bmi = w / h ^ 2 ]

getFatBMIs xs = [bmiTell' bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi > 25.0 ]

-- CASE EXPRESSION: pattern matching in function definitions is syntactic sugar for case expressions
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
        where what [] = "empty."  
              what [x] = "a singleton list."  
              what xs = "a longer list."  