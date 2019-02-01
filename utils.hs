module Utils where

import System.Environment

main = do
        contents <- readFile "test.txt"
        print . f . foldr (+) 0 $ map read contents

f :: Int -> Int
f i = i+1

readInt :: String -> Int
readInt = read
