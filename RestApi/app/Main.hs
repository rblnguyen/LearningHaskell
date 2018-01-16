{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO(readFile)
import Data.Time(getCurrentTime)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText,encode, decode)
import Data.Aeson.Types (typeMismatch, ToJSONKey, FromJSONKey)
import Data.Text (Text)
import Lib


main :: IO ()
main = do
    --printWords
    --printWordsBind
    -- printWordsDo >>= \ n -> putStrLn ("See you " ++ n)
    -- name <- printWordsDo
    -- putStrLn ("See You again " ++ name)
    fmap seeYou printWordsDo >>= putStrLn
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

