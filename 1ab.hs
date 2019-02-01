import System.IO
import Data.List.Split

mainA = do
          contents <- readFile "1ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents

mainB = do
          contents <- readFile "1ab.txt"
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
--positive Ints cant have '+' sign
parse :: String -> [Int]
parse str = map read $ splitOn "\n" $ filter (/= '+') str :: [Int]

execA :: [Int] -> String
execA ns = show $ sum ns

execB :: [Int] -> String
execB ns = show . firstDuplicate . partialSum . cycle $ ns

partialSum' :: [Int] -> [Int]
partialSum' = scanl (+) 0

partialSum :: [Int] -> [Int]
partialSum (lastFreqs:firstChange:changes) = sum : partialSum (sum:changes)
                                        where
                                          sum = lastFreqs + firstChange

firstDuplicate :: [Int] -> Int
firstDuplicate = helperDuplicate []

helperDuplicate :: [Int] -> [Int] -> Int
helperDuplicate prevs (n:ns)
                              | n `elem` prevs = n
                              | otherwise = helperDuplicate (prevs ++ [n]) ns