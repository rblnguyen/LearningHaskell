module FFOMonad 
where

import Data.Char  
import Data.List
import Control.Applicative

ffoMonadModuleName :: IO ()
ffoMonadModuleName = putStrLn "FFOMonad: Fist Full Of Monad"

-- similar to the bind function >>= in Monad
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x
-- Just 3 `applyMaybe` \x -> Just (x+1)
-- Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
-- Nothing `applyMaybe` \x -> Just (x+1)
-- Nothing `applyMaybe` \x -> Just (x ++ " :)")  
