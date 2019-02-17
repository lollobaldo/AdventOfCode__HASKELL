module Main where

import Data.Char
import Data.List
import Data.Ord

type Unit = Char
type Polymer = [Unit]

main = do
          contents <- readFile "5.txt"
          print . execA . parse $ contents
          print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> Polymer
parse str = str

execA :: Polymer -> Int
execA = length . reactor

execB :: Polymer -> Int
execB = tryAll ['a'..'z']

reactor :: Polymer -> Polymer
reactor = foldl' reactor' []
            where
              reactor' :: Polymer -> Unit -> Polymer
              reactor' [] x = [x]
              reactor' acc@(y:_) x
                  | checkIfReact x y = tail acc
                  | otherwise        = x:acc

checkIfReact :: Unit -> Unit -> Bool
checkIfReact x y = (toLower x == toLower y) && x /= y

tryAll :: String -> String -> Int
tryAll alph str = foldl' (\mi ch -> min mi $ length . reactor . remover str $ ch) (10000) alph

remover :: String -> Char -> String
remover str ch = filter ((ch /=) . toLower) str