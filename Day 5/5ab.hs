module Main where
import Data.List.Split
import Data.Char (isDigit)

main = do
          contents <- readFile "3ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
parse :: String -> [Piece]
parse str = [(id, x, y, a, b) | [id,x,y,a,b] <- map (map read . filter isNumber . splitOneOf "# @,:x") $ lines str]

execA :: [Piece] -> String
execA = show . countConflicts

execB :: [Piece] -> String
execB = show . checkFree [] . formatter