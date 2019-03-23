module Main where

import Control.Monad
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List.Split
-- import Data.List
-- import Data.Char

main = do
          contents <- liftM parse . readFile $ "11.txt"
          print . execA $ contents
          print . execB $ contents

parse :: String -> Int
{-^ Parse input into usable data structure-}
parse str = read str

execA :: Int -> ((Int, Int), (Int, Int))
{-^
    1. 'play' the sky 1sec at a time until reaching minimum y-box;
    2. Draw the sky;
    3. Return string.
-}
execA sn = getMax . putMaxSquare . getArea $ pows
  where
    pows = Map.fromList $ [((x,y) , getPower (x,y) sn) | x <- [1..300], y <- [1..300]]

execB :: Int -> Int
{-^ Not needed, done in trace in 'play' -}
execB = id

getPower :: (Int, Int) -> Int -> Int
getPower (x,y) sn = (((x+10)*y + sn)*(x+10) `div` 100) `mod` 10 - 5

-- putArea :: Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
-- putArea map = Map.mapWithKey (getArea map) map

-- getArea :: Map.Map (Int,Int) Int -> (Int, Int) -> Int -> Int
-- getArea map (x,y) _ = sum [map Map.! (a,b) | a <- [1..x], b <- [1..y]]

getArea :: Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
getArea ori = Map.foldrWithKey' adder Map.empty ori
    where
      adder :: (Int,Int) -> Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
      adder c@(x,y) v map = Map.insert (x,y) (total c v map) map -- trace (show (x,y)) Map.insert (x,y) (total c v map) map

      total :: (Int,Int) -> Int -> Map.Map (Int, Int) Int -> Int
      total (300,300) v _ = v
      total (x,y) v map
        | x == 300 = sum [ori Map.! (300,b) | b <- [y..300]]
        | y == 300 = sum [ori Map.! (a,300) | a <- [x..300]]
        | x < y  = sum $ (map Map.! (x+1,y)) : [ori Map.! (x,b) | b <- [y..300]]
        | True   = sum $ (map Map.! (x,y+1)) : [ori Map.! (a,y) | a <- [x..300]]

putMaxSquare :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) (Int, Int)
putMaxSquare map = Map.mapWithKey maxx map
  where
    maxx c@(x,y) _ = maximum [(getSquare map c s, s) | s <- [0..300-(max x y)]]

getSquare :: Map.Map (Int, Int) Int -> (Int, Int) -> Int -> Int
getSquare map (x,y) s = a - b - c + d
  where
    r = s-0
    a = map Map.! (x   , y  )
    b = map Map.! (x+r , y  )
    c = map Map.! (x   , y+r)
    d = map Map.! (x+r , y+r)

-- getSquare :: Map.Map (Int,Int) Int -> (Int, Int) -> Int -> Int
-- getSquare map (x,y) _ 
--   | x > 298  || y > 298 = 0
--   | otherwise = (map Map.! (x,y  )) + (map Map.! (x+1,y  )) + (map Map.! (x+2,y  ))
--               + (map Map.! (x,y+1)) + (map Map.! (x+1,y+1)) + (map Map.! (x+2,y+1))
--               + (map Map.! (x,y+2)) + (map Map.! (x+1,y+2)) + (map Map.! (x+2,y+2))

getMax :: Map.Map (Int, Int) (Int, Int) -> ((Int, Int), (Int,Int))
getMax = Map.foldrWithKey (\k (x,s') (c,(v,s)) -> if x>v then (k,(x,s')) else (c,(v,s))) ((0,0),(minBound,0))

-- getMax :: Map.Map (Int, Int) Int -> ((Int, Int), Int)
-- getMax = Map.foldrWithKey (\k x (c,v) -> if x>v then (k,x) else (c,v)) ((0,0),minBound)