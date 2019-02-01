import System.IO
import Data.List.Split

mainA = do
          contents <- readFile "1ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents

mainB = do
          contents <- readFile "1ab.txt"
          putStrLn contents

parse :: String -> [Int]
parse str = map readNeg $ splitOn "\n" str :: [Int]
              where
                readNeg :: String -> Int
                readNeg ('+':ns) = read ns
                readNeg ns = read ns

myFunc :: String -> Int
myFunc s = read s

execA :: [Int] -> String
execA ns = show $ foldr (+) 0 ns