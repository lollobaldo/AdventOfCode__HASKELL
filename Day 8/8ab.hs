module Main where

--import Data.List.Split
import Data.List
import Data.Char (isUpper)

data Node = Node [Node] [Int] deriving (Show)

main = do
          contents <- readFile "8ab.txt"
          print . parse $ contents
          --print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> Node
parse str = populateTree $ map read $ words str

execA :: [Int] -> Int
execA = undefined

execB :: [Int] -> String
execB = undefined

populateTree :: [Int] -> Node
populateTree [] = Node [] []
populateTree (nc:nm:xs) = Node [populateTree childs] metas
                            where
                              childa 0 x = []
                              childs n x = parse x : (childa (n-1) (drop (size $ parse x) x))
                              --childs = fst splitted
                              metas  = snd splitted
                              splitted = splitAt (length xs - nm) xs