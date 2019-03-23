module Main where

import Data.Set (Set, empty, insert , member)
import Control.Monad

main = do
          contents <- liftM parse . readFile $ "1.txt"
          print . execA $ contents
          print . execB $ contents

--parse by line based on '\n' Char
--positive Ints cant have '+' sign
parse :: String -> [Int]
parse = map read . lines . filter (/= '+')

execA :: [Int] -> Int
execA = sum

execB :: [Int] -> Int
execB = firstDuplicate . partialSum . cycle

partialSum :: [Int] -> [Int]
partialSum = scanl (+) 0

firstDuplicate :: [Int] -> Int
firstDuplicate = helperDuplicate empty

helperDuplicate :: Set Int -> [Int] -> Int
helperDuplicate prevs (n:ns)
                              | n `member` prevs = n
                              | otherwise = helperDuplicate (insert n prevs) ns