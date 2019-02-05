module Main where

import Data.Char

type Unit = Char
type Polymer = [Unit]

main = do
          contents <- readFile "5ab.txt"
          print $ length $ reduce contents
          print $ foldl1 min $ (\c -> length $ reduce $ filter (\d -> (toLower d) /= c) contents) <$> ['a'..'z']
          --print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> Polymer
parse str = str

execA :: Polymer -> Int
execA = length . whileChanges reactor

execB :: Polymer -> Int
execB = minimum . tryAll ['a'..'z']

reactor :: Polymer -> Polymer
reactor [] = []
reactor [a] = [a]
reactor (x:y:z:zs)
                    | checkIfReact x y = reactor zs
                    | otherwise        = x : reactor (y:zs)

checkIfReact :: Unit -> Unit -> Bool
checkIfReact a b = (toLower a == toLower b) && isUpper a /= isUpper b

whileChanges :: Eq a => (a -> a) -> a -> a
whileChanges f str
                    | str == new = str
                    | otherwise  = whileChanges f new
                        where
                          new = f str

tryAll :: String -> String -> [Int]
tryAll (ch:alph) str = [length . whileChanges reactor . remover ch $ str | ch <- str]

remover :: Char -> String -> String
remover ch str = filter ((ch /=) . toLower) str

reduce l = foldl reduce' [] l
    where reduce' [] x = [x]
          reduce' acc@(y:_) x
            | x /= y && (toLower x) == (toLower y) = tail acc
            | otherwise                            = x:acc