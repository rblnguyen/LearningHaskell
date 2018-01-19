module TypeAndTypeClasses
    ( removeNonUppercase
    , addThree
    , circumference
    ) where

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c 

circumference :: Float -> Float
circumference r = 2 * pi * r