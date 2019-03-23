import os

# os.mldir("Days")
for i in range(25):
	j = i+1
	os.mkdir("Day {0}".format(j))

	f = open("Day {0}/{0}.txt".format(j), "w+")
	f.close()
	f = open("Day {0}/{0}.hs".format(j), "w+")

	template = """module Main where

import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as S
import Control.Monad

main = do
          contents <- liftM parse . readFile $ "{0}.txt"
          print . execA $ contents
          -- print . execB $ contents

parse :: String -> []
--^ Parse into usable data structure
parse = id
-- parse str = [(x, y, u, v) | [x,y,u,v] <- map (map read . filter isNumber . splitOneOf "<,>") $ lines str]

execA :: [] -> Int
execA = id

execB :: [] -> Int
execB = undefined"""

	f.write(template.format(j))