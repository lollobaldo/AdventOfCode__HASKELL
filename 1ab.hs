module Main where
--import Data.List.Split
import Data.Set hiding (map,filter)


main = do
          contents <- readFile "1ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
--positive Ints cant have '+' sign
parse :: String -> [Int]
parse str = map read $ lines $ filter (/= '+') str

execA :: [Int] -> String
execA = show . sum

execB :: [Int] -> String
execB = show . firstDuplicate . partialSum . cycle

partialSum :: [Int] -> [Int]
partialSum = scanl (+) 0

firstDuplicate :: [Int] -> Int
firstDuplicate = helperDuplicate empty

helperDuplicate :: Set Int -> [Int] -> Int
helperDuplicate prevs (n:ns)
                              | n `member` prevs = n
                              | otherwise = helperDuplicate (insert n prevs) ns