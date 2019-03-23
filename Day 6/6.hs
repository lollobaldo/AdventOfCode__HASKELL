module Main where
import Data.List.Split
import Data.Char (isDigit)

type Fabric = [[SquareInch]]
type ID = Int
type Piece = (ID, Int, Int, Int, Int)

data SquareInch = Free | Taken | Conflict deriving (Eq,Show)

main = do
          contents <- readFile "6.txt"
          print . execA . parse $ contents
          print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> [Piece]
parse str = [(id, x, y, a, b) | [id,x,y,a,b] <- map (map read . filter isNumber . splitOneOf "<>,") $ lines str]

isNumber :: String -> Bool
isNumber str = (all isDigit) str && (not . null) str

execA :: [Piece] -> Int
execA = countConflicts

execB :: [Piece] -> Int
execB = checkFree [] . formatter

fabric :: Fabric
fabric = replicate 1001 $ replicate 1001 Free

insert :: Fabric -> Piece -> Fabric
insert fab (_,x,y,w,h) = [if not (i > y && i <= (y+h))
                          then row
                          else
                            [if not (j > x && j <= (x+w))
                              then col
                              else
                                if col == Taken then Conflict
                                  else if col == Free then Taken else col
                            | (j,col) <- zip [1..] row]
                        | (i,row) <- zip [1..] fab]

insertAll :: Fabric -> [Piece] -> Fabric
insertAll fab [] = fab
insertAll fab (p:ps) = insertAll (insert fab p) ps

countConflicts :: [Piece] -> Int
countConflicts ps = length [x | x <- (concat (insertAll fabric ps)) , x == Conflict]

checkFree :: [Piece] -> [Piece] -> Int
checkFree _ [] = 0
checkFree pre (p@(id,_,_,_,_):ps)
              | checkIntersect p (pre++ps) = id
              | otherwise = checkFree (pre++[p]) ps

formatter ps = [format p | p <- ps]
format (id,x,y,w,h) = (id,x+1,y+1,x+w,y+h)

checkIntersect :: Piece -> [Piece] -> Bool
checkIntersect p@(_,x,y,w,h) ps = and [notIntersect p s | s <-ps]

notIntersect :: Piece -> Piece -> Bool
notIntersect (_,x,y,w,h) (_,a,b,c,d) = (x > c || a > w ||
                                        y > d || b > h)