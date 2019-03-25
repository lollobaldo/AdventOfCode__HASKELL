module Main where

import Control.Monad
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.IntMap.Lazy as Map
import Data.List.Split
import Data.List
import Data.Algorithms.KMP
-- import Data.Char

data Scoreboard = Score {
  cs :: Int,
  p1 :: Int,
  p2 :: Int,
  sc :: Map.IntMap Int
} deriving Show

initialScoreboard = Score {cs=2,p1=0,p2=1,sc=Map.fromAscList (zip [0..] [3,7])}

main = do
          contents <- liftM parse . readFile $ "14.txt"
          print . execA $ contents
          print . execB $ contents

parse :: String -> Int
{-^ Parse input into usable data structure-}
parse str = read str

execA :: Int -> Int
{-^
    1. 'play' the sky 1sec at a time until reaching minimum y-box;
    2. Draw the sky;
    3. Return string.
-}
execA = getScore

execB :: Int -> Int
{-^ Not needed, done in trace in 'play' -}
execB imp = result
  where
    word = (reverse . digits) imp
    text = getScores
    kmpTable = build word
    result = head $ match kmpTable text

digits :: Integral x => x -> [x]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)


getScore :: Int -> Int
getScore n = (foldl' (\acc v -> 10*acc+v) 0 . take 10 . drop n . map snd . Map.toAscList . sc . nthScoreboard (n)) initialScoreboard

getScores :: [Int]
getScores = (map snd . Map.toAscList . sc . nthScoreboard (100000000)) initialScoreboard

nthScoreboard :: Int -> Scoreboard -> Scoreboard
nthScoreboard n scb
  | cs scb >= n+10 = scb
  | otherwise         = nthScoreboard n (nextScoreboard scb)

nextScoreboard :: Scoreboard -> Scoreboard
nextScoreboard scb = Score{cs=size',p1=nn (p1 scb) r1, p2=nn (p2 scb) r2, sc=news (sc scb)}
  where
    size = cs scb
    size'
      | sum >= 10 = size+2
      | otherwise = size+1
    (r1,r2) = ((sc scb Map.! p1 scb) , (sc scb Map.! p2 scb))
    nn a b = (`mod` size') . (+1) . (+a) $ b
    sum = r1+r2
    news
      | sum >= 10 = (Map.insert size 1) . Map.insert (size+1) (sum `mod` 10)
      | otherwise = Map.insert size (sum `mod` 10)