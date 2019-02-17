module Main where

import Data.List.Split
import Data.List
import Data.Char (isUpper, isDigit)

main = do
          contents <- readFile "18.txt"
          print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '
' Char
parse :: String -> []
parse str = [(x, y, u, v) | [x,y,u,v] <- map (map read . filter isNumber . splitOneOf "<,>") $ lines str]

isNumber :: String -> Bool
isNumber str = (all (`elem` " +-0123456789")) str && (not . null) str

execA :: [] -> Int
execA = id

execB :: [] -> Int
execB = undefined