module Parser
       (
        parser
        ,lucky
        ,numberWords
        ,removeNoneUpperCase
        ,addThreeNumber
       )where
    
import Lib

parser :: IO ()
parser = Lib.outPutGrid grid

lucky::(Integral a) => a -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky _ = "NO DICE"    

numberWords :: (Integral a) => a -> String
numberWords 1 = "One!"  
numberWords 2 = "Two!"  
numberWords 3 = "Three!"  
numberWords 4 = "Four!"  
numberWords 5 = "Five!"  
numberWords _ = "Not between 1 and 5" 

removeNoneUpperCase :: [Char] -> [Char]
removeNoneUpperCase st = [c | c <- st, c `elem` ['A' ..'Z']]


removeNoneUpperCaseString :: String -> String
removeNoneUpperCaseString st = [c | c <- st, c `elem` ['A' ..'Z']]

addThreeNumber :: Int -> Int -> Int -> Int
addThreeNumber a b c = a + b + c

