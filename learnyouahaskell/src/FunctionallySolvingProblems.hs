module FunctionallySolvingProblems
where 

import Data.List

functionallySolvingProblems = putStrLn "FunctionallySolvingProblems"

--------------------------------------------------
---- Reverse Polish Notation Calculator (RPN) ----

--   RPN: 10 4 3 + 2 * - = ?  
--   Think of a stack. Go over the expression from left to right. Every time a number is encountered, push it on to the stack.
--   When we encounter an operator, take the two numbers that are on top of the stack (we also say that we pop them), 
--   use the operator and those two and then push the resulting number back onto the stack. When you reach the end of the expression, 
--   you should be left with a single number if the expression was well-formed and that number represents the result.
calculateRPN :: (Num a) => a
calculateRPN = 10

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs
            
solveRPN' :: (Num a, Read a) => String -> a
solveRPN' input = 
          let values = words input
          in head (foldl foldingFunction [] values)
            where foldingFunction (x:y:ys) "*" = (x * y):ys  
                  foldingFunction (x:y:ys) "+" = (x + y):ys  
                  foldingFunction (x:y:ys) "-" = (y - x):ys  
                  foldingFunction xs numberString = read numberString:xs  


