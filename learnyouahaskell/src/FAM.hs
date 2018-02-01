-- Functors_applicatives_monads
module FAM 
where

import Data.Char  
import Data.List

famModuleName :: IO ()
famModuleName = putStrLn "F.A.M: Functors; Applicatives and Monads"

functorsFunc :: IO()
functorsFunc = do
  putStrLn "Type something"  
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line