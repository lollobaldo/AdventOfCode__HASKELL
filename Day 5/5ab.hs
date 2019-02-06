module Main where

import Data.Char
import Data.List

type Unit = Char
type Polymer = [Unit]

main = do
          contents <- readFile "5ab.txt"
          print . execA . parse $ contents
          print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> Polymer
parse str = str

execA :: Polymer -> Int
execA = length . reactor

execB :: Polymer -> Int
execB = minimum . tryAll ['a'..'z']

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

whileChanges :: Eq a => (a -> a) -> a -> a
whileChanges f str
                    | str == new = str
                    | otherwise  = whileChanges f new
                        where
                          new = f str

tryAll :: String -> String -> [Int]
tryAll alph str = [length . reactor . remover ch $ str | ch <- alph]

remover :: Char -> String -> String
remover ch str = filter ((ch /=) . toLower) str