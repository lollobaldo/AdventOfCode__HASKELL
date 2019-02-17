module Main where

import Data.List.Split
import Data.List
import Data.Char (isUpper, isDigit)

type Point = (Int, Int, Int, Int)

main = do
          contents <- readFile "9.txt"
          print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> [Point]
parse str = [(x, y, u, v) | [x,y,u,v] <- map (map read . filter isNumber . splitOneOf "<,>") $ lines str]

isNumber :: String -> Bool
isNumber str = (all (`elem` " +-0123456789")) str && (not . null) str

execA :: [Point] -> [Point]
execA = id

execB :: [Point] -> String
execB = undefined