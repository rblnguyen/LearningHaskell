import Data.List

sayHello :: IO()
sayHello = putStrLn "Hello World"

main :: IO()
main = 
    do 
        putStrLn "Begin read file"
        content <- readFile "numbers.txt"
        putStrLn content
        putStrLn "End of read file"

        let values = readInts content
            count = length values
            total = sum values
            mean = fromIntegral total / fromIntegral count
            range = minMax values
        print count
        print total
        print mean
        print range
            
        putStrLn $ formatList "<list>" "</list>" "|" ["a", "a", "a", "a"]

readInts :: String -> [Int]
readInts s = 
    let 
        ws = words s 
    in 
        map read ws ::[Int]

minMax :: Ord a => [a] -> Maybe (a, a)
minMax (h : t) = Just $ foldr
    (\x (min, max) -> (if x < min then x else min, if x > max then x else max))
    (h, h)
    t
minMax _ = Nothing

formatList :: String -> String -> String -> [String] -> String
formatList start end sep xs = start ++ intercalate sep (map show xs) ++ end

