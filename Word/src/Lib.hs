module Lib
    ( grid
    , languages
    , formatGrid
    , outPutGrid
    , findWord
    , findWords
    , findWordInLine
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
import Data 

type Grid = [String]


formatGrid :: Grid -> String
formatGrid = unlines

outPutGrid :: Grid -> IO()
outPutGrid grid = putStrLn (formatGrid grid)

getLines :: Grid -> [String]
getLines grid = --grid ++ map reverse grid
   let horizontal = grid
       vertical = transpose grid
       diagonal1 = diagonalize grid
       diagonal2 = diagonalize (map reverse grid)
       lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ map reverse lines

diagonalize :: Grid -> Grid
--diagonalize grid = transpose (skew grid)
diagonalize grid = (transpose.skew) grid

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line


findWord :: Grid -> String -> Maybe String
findWord grid word = 
    let lines = getLines grid 
        found = or $map (findWordInLine word) lines
    in 
        if found 
        then 
            Just word 
        else
            Nothing


findWords :: Grid -> [String] -> [String]
findWords grid words = --catMaybes (map (findWord grid) words)
    let foundWords =  map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

