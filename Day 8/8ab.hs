module Main where

--import Data.List.Split
import Data.List
import Data.Char (isUpper)

data Node = Node [Node] [Int] deriving (Show)

main = do
          contents <- readFile "8ab.txt"
          --print . parse $ contents
          print . execA . parse $ contents
          print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> Node
parse str = populateTree $ map read $ words str

execA :: Node -> Int
execA = sumMetas

execB :: Node -> Int
execB = getValue

sumMetas :: Node -> Int
sumMetas (Node childs metas) = sum metas + sum (map sumMetas childs)

nodeLength :: Node -> Int
nodeLength (Node childs metas) = 2 + nodeLength' childs + length metas
nodeLength' = sum . map nodeLength

getNthChild :: [Node] -> Int -> Maybe Node
getNthChild _      0 = Nothing
getNthChild []     _ = Nothing
getNthChild (x:_)  1 = Just x
getNthChild (_:xs) i = getNthChild xs (i-1)

getValue :: Node -> Int
getValue (Node [] metas) = sum metas
getValue (Node childs metas) = sum . map (maybe 0 getValue . getNthChild childs) $ metas

populateTree :: [Int] -> Node
populateTree [] = Node [] []
populateTree (nc:nm:xs) = Node (childs nc xs) metas
                            where
                              childs 0          xs = []
                              childs childsLeft xs = populateTree xs : childs (childsLeft - 1) (drop (nodeLength $ populateTree xs) xs)
                              metas = take nm $ drop (nodeLength' $ childs nc xs) xs