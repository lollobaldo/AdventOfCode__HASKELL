module Main where

import Control.Monad
import Debug.Trace
import qualified Data.Set as Set
import Data.List.Split
-- import Data.List
-- import Data.Char

type Star = (Int, Int, Int, Int)
type Sky = [(Int,Int)]

main = do
          contents <- liftM parse . readFile $ "10.txt"
          putStr . execA $ contents -- putStr required as newlines are output
          print . execB $ contents

parse :: String -> [Star]
{-^ Parse input into usable data structure-}
parse str = [(x, y, vx, vy) | [x,y,vx,vy] <- map (map read . filter isNumber . map trim . splitOneOf "<>,") $ lines str]
  where
    isNumber str = (all (`elem` "-0123456789")) str && (not . null) str
    trim = dropWhile (== ' ')

execA :: [Star] -> String
{-^
    1. 'play' the sky 1sec at a time until reaching minimum y-box;
    2. Draw the sky;
    3. Return string.
-}
execA = drawSky . play 0

play :: Int -> [Star] -> Sky
{-^ Compare dimension of this sky with the next until minimum is found.-}
play n stars
  | (snd . getDim) sky < (snd . getDim) nextSky = trace (show n) sky
  | otherwise = play (n+1) stars
    where
      sky = getSky n stars
      nextSky = getSky (n+1) stars

execB :: [Int] -> Int
{-^ Not needed, done in trace in 'play' -}
execB = undefined

getSky :: Int -> [Star] -> Sky
{-^ Draw sky after nth second-}
getSky sec = map (\(x,y,vx,vy) -> (x+sec*vx,y+sec*vy))

getDim :: Sky -> (Int, Int)
{-^ Get dimensions of spanning box-}
getDim sky = (c-a,d-b)
  where
    (a,b,c,d) = foldr (\(x,y) (a,b,c,d) -> (min x a, min y b, max x c, max y d)) (maxBound, maxBound, minBound, minBound) sky

drawSky :: Sky -> String
{-^ Draw a given sky into formatted string-}
drawSky sky = unlines [[if ((x,y) `elem` sky) then '#' else '.' | x <- [x1..x2]] | y <- [y1..y2]]
  where
    --TODO: DRY, no time now, might come back to it later
    (x1,y1,x2,y2) = foldr (\(x,y) (a,b,c,d) -> (min x a, min y b, max x c, max y d)) (maxBound, maxBound, minBound, minBound) sky