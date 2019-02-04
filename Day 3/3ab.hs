module Main where
import Data.List.Split
import Data.Char (isDigit)

type Fabric = [[SquareInch]]
type ID = Int
type Piece = (ID, Int, Int, Int, Int)
type Pieces = [Piece]

data SquareInch = Free | Taken | Conflict deriving (Eq,Show)

main = do
          contents <- readFile "3ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
parse :: String -> [Piece]
parse str = [(id, x, y, a, b) | [id,x,y,a,b] <- map (map read . filter isNumber . splitOneOf "# @,:x") $ lines str]

isNumber :: String -> Bool
isNumber str = (all isDigit) str && (not . null) str

-- parserHelper :: [String] -> Piece
-- parserHelper [_,_,_,x,y,_,a,b] = ()

execA :: [Piece] -> String
execA = show . countConflicts

execB :: [Piece] -> String
execB ps = "head"

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

final :: Fabric -> Pieces -> Fabric
final fab [] = fab
final fab (p:ps) = final (insert fab p) ps

countConflicts :: Pieces -> Int
countConflicts ps = length [x | x <- (concat (final fabric ps)) , x == Conflict]

-- final :: Pieces -> Pieces -> Int
-- final _ [] = 0
-- final pre (p@(id,_,_,_,_):ps)
--               | traceShow (p) (check p (pre++ps)) = id
--               | otherwise = final (pre++[p]) ps

-- formatter ps = [format p | p <- ps]
-- format (id,x,y,w,h) = (id,x+1,y+1,x+w,y+h)

-- check :: Piece -> Pieces -> Bool
-- check p@(_,x,y,w,h) ps = and [notIntersect p s | s <-ps]

-- notIntersect :: Piece -> Piece -> Bool
-- notIntersect (_,x,y,w,h) (_,a,b,c,d) = (x > c || a > w ||
--                                         y > d || b > h)