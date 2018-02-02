
module Main where

import Control.Monad
import Control.Exception
import Data.Typeable (TypeRep, Typeable, typeRep)
import System.Environment
import System.IO;
import System.IO.Error; 
import System.Directory
import Text.Read     (readMaybe)


newtype MyException = NoParseException String deriving (Show, Typeable)
instance Exception MyException

-- Prompt consists of two functions:
-- The first converts an output paramter to String being printed to the screen.
-- The second parses user's input.
data Prompt o i = Prompt (o -> String) (String -> Either MyException i)

main :: IO ()
main = catch mainFunc handler
    where 
        handler :: IOError -> IO ()
        handler e  
            | isDoesNotExistError e = 
                case ioeGetFileName e of 
                    Just path -> putStrLn $ "File does not exist at: " ++ path
                    Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
            | isUserError e = putStrLn "User Error"
            | otherwise = do 
                putStrLn "Unhandled exception"
                ioError e
        --handler ex = putStrLn $ "Caught Exception: " ++ show ex

mainFunc :: IO()
mainFunc = do
    putStrLn "Begin Executing Main"

    putStrLn "Enter the filename"
    name <- getLine 
    contents <- readFile name 
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    
    --catch (runPrompt myPrompt ()) handleEx >>= putStrLn
    -- runPrompt myPrompt () >>= either handleEx return >>= putStrLn
    -- putStrLn =<< either handleEx return =<< runPrompt myPrompt ()
    -- divByZero
    -- divByZero1
    -- divByZero2
    putStrLn "End of Main"

countLine :: String -> IO ()
countLine fileName = do     
    exists <- doesFileExist fileName
    if exists    
    then do
        contents <- readFile fileName
        putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    else 
        putStrLn "The file doesn't exist!"

-- runPrompt accepts a Prompt and an output parameter. It converts the latter
-- to an output string using the first function passed in Prompt, then runs
-- getline and returns user's input parsed with the second function passed
-- in Prompt.
runPrompt :: Prompt o i -> o -> IO (Either MyException i)
runPrompt (Prompt ofun ifun) o = do
        putStr (ofun o)
        hFlush stdout
        fmap ifun getLine

myPrompt = Prompt (const "> ") (\s -> 
    if null s
    then Left $ NoParseException s
    else Right s)

handleEx :: MyException -> IO String
handleEx (NoParseException s) = return $ "Illegal string: " ++ s

divByZero = do
    result <- tryJust selectDivByZero (evaluate $ 5 `div` 0)
    case result of
        Left ex -> putStrLn $ "Caught Exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
  where
    selectDivByZero :: ArithException -> Maybe String
    selectDivByZero DivideByZero = Just "zero"
    selectDivByZero _ = Nothing 

divByZero1 = do
    result <- try (evaluate $ 5 `div` 0) :: IO (Either ArithException Int)
    case result of
        Left ex -> putStrLn $ "Caught Exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
   
divByZero2 = catch( print $ 5 `div` 0) handler
    where 
        handler :: ArithException -> IO ()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex
