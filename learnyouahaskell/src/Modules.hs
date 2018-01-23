module Modules
where

import Data.List

moduleFunc = putStrLn "Modules"

uniqueList xs = Data.List.nub xs

-- Data.List: 
-- intersperse: takes an element and a list and then puts that element in between each pair of elements in the list.
result = intersperse '.' "MONKEY" -- -> M.O.N.K.E.Y

--intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
result1 = intercalate [0,0] [[1,2],[3,4],[5,6]] -- -> [1,2,0,0,3,4,0,0,5,6]

--concat flattens a list of lists into just a list of elements.
result2 = concat [[3,4,5],[2,3,4],[2,1,1]] 

