module IOmodule where

import System.IO(readFile)
import Data.Time(getCurrentTime)
import Data.Aeson (encode, decode)
import Data.Char

printOnScreen :: IO ()
printOnScreen = do
    -- printWords
    -- printWordsBind
    -- printWordsDo >>= \ n -> putStrLn ("See you " ++ n)
    -- name <- printWordsDo
    -- putStrLn ("See You again " ++ name)
    -- fmap seeYou printWordsDo >>= putStrLn
    putStrLn "Something text"
    printConfig >>= putStrLn
    printTime >>= putStrLn
    encodeList
    printSomething

printWords = do 
    putStr "Hello"
    putStr " "
    putStr "World!"
    putStrLn ""

printWordsDo = do 
    putStr "What is your first name? "
    first <- getLine
    putStr "And your last name? "
    last <- getLine
    let full = first ++ " " ++ last
    putStrLn ("Pleased to meet you, " ++ full ++ "!")
    return full

printWordsBind =
    putStr "What's your first name? " >> getLine >>= \ fname -> 
        putStr "And your last name? " >> getLine >>= (\lname -> 
            let full = fname ++ " " ++ lname 
            in putStrLn full)

seeYou name = "See you " ++ name ++ "!"


printConfig = do 
    contents <- readFile "stack.yaml"
    return contents

printTime = do 
    time <- getCurrentTime
    return (show time)

-- list :: [Int]
-- list = [1,2,3,4]
encodeList = do 
    let list = [1,2,3,4]::[Int]
    return (encode list)

printSomething = do
    let action = putStrLn "Print something"

    action
    return()

beCareful :: Maybe Int
beCareful = do
    Just 6
    Nothing
    return 5

data Operator = Plus | Minus | Times | Div | Unknown
    deriving (Show, Eq)

operator :: Char -> Operator
operator c  | c == '+' = Plus
            | c == '-' = Minus
            | c == '*' = Times
            | c == '/' = Div
            | otherwise = Unknown

data Token = TokOp Operator | TokIndet String | TokNum Int
        deriving (Show, Eq)
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) | c `elem` "+-*/" = TokOp(operator c )  : tokenize cs
                    | isDigit c = TokNum (digitToInt c) : tokenize cs
                    | isAlpha c = TokIndet [c]          : tokenize cs
                    | isSpace c = tokenize cs
                    | otherwise = error $ "Can't tokenize " ++ [c]



-- tokenize1 :: String -> [Token]
-- tokenize1 [] = []
-- tokenize1 c `elem` "+-*/" = TokOp(operator c )
-- tokenize1 isDigit c = TokNum (digitToInt c)
-- tokenize1 isAlpha c = TokIndet [c]
-- tokenize1 isSpace c = return ()

------------------------------
-- Fibonaci numbers
------------------------------
printFib :: Int -> [Int]
printFib n = fmap fib' [1..n]

fib :: Int -> Int
fib n = if n > 2 then fib (n-1) + fib (n -2) else 1

fib' :: Int -> Int
fib' n 
        | n > 2 = fib (n - 1) + fib (n -2)
        | otherwise = 1

cat:: [a] -> [a] -> [a]
cat xs ys = foldr (:) ys xs

toInt::String -> [Int]
toInt [] = []
toInt cs = fmap digitToInt cs

sumDigits :: String -> Int
sumDigits [] = 0
sumDigits cs = sum $ toInt cs 