import os
for i in range(25):
    j = i+1
    os.mkdir("Day {0}".format(j))

    f = open("Day {0}/{0}.txt".format(j), "w+")
    f.close()
    f = open("Day {0}/{0}.hs".format(j), "w+")

    template = """module Main where

import Data.List.Split
import Data.List
import Data.Char (isUpper, isDigit)

main = do
          contents <- readFile "{0}.txt"
          print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> []
parse str = [(x, y, u, v) | [x,y,u,v] <- map (map read . filter isNumber . splitOneOf "<,>") $ lines str]

isNumber :: String -> Bool
isNumber str = (all (`elem` " +-0123456789")) str && (not . null) str

execA :: [] -> Int
execA = id

execB :: [] -> Int
execB = undefined"""

    f.write(template.format(j))
