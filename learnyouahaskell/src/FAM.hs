-- Functors_applicatives_monads
module FAM 
where

import Data.Char  
import Data.List
import Control.Applicative

famModuleName :: IO ()
famModuleName = putStrLn "F.A.M: Functors; Applicatives and Monads"

functorsFunc :: IO()
functorsFunc = do
  putStrLn "Type something"  
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

func0 :: IO()
func0 = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a


-- <$> is fmap; 
-- <*> is "apply to"
func1 :: [String]
func1 = (\x y z -> x  ++ y ++ z) <$> ["Hello"] <*> [" - "] <*> ["World"]

func2 :: [Int]
func2 = filter (> 5) $(\x y-> 2 * x * y) <$> [2,5,10] <*> [8,10,11]

--ZipList
zipList1 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]

zipList2 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
zipList3 = getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  

-- (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y).
zipList4 :: [(Char, Char, Char)]
zipList4 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  

-- zipWith takes a function that takes two parameters and zips two lists with it. 
-- zipWith3 takes a function that takes three parameters and zips three lists with it, and so on, up to 7

-- lifA2 similar to liftA2 f a b = f <$> a <*> b 
-- a short hand for <$> a <*> b
liftA2Func1 = liftA2 (:) (Just 2) (Just [4]) 

-- Is the same as 
liftA2Func2 = (:) <$> Just 2 <*> Just [4]