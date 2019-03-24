module Main where

import Control.Monad
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as Map
import Data.List.Split
import Data.List
-- import Data.Char

type Pots = Map.IntMap Bool
type Rule = [Bool]

main = do
          contents <- liftM parse . readFile $ "12.txt"
          print . execA $ contents
          putStr . execB $ contents

parse :: String -> (Pots, [Rule])
{-^ Parse input into usable data structure-}
parse str = (Map.union toAdd getPot, getRules)
  where
    getPot = (Map.fromAscList
            . zip [0..]
            . map (== '#')
            . drop 2
            . dropWhile (/= ':')
            . head
            . lines) str
    toAdd :: Pots
    toAdd = Map.fromAscList $ zip [min-2,min-1,max+1,max+2] (repeat False)
    min = (fst . Map.findMin) getPot
    max = (fst . Map.findMax) getPot
    getRules :: [Rule]
    getRules = (map (take 5) . filter (last)) [map (== '#') r | r <- (drop 2 . lines) str]
    -- formatRule :: String -> Rule
    -- formatRule = take 5 . filter (tail) . map (== '#')

-- execA :: (Pots, [Rule]) -> String
{-^
    1. 'play' the sky 1sec at a time until reaching minimum y-box;
    2. Draw the sky;
    3. Return string.
-}
execA (p,r) = sumIs . getGen 20 r $ p

execB :: (Pots, [Rule]) -> String
{-^ Not needed, done in trace in 'play' -}
execB (p,r) = unlines $ map show diffs
  where
    diffs = zipWith (\(a,b) (c,d) -> (c, d-b, d)) x (tail x)
    x = getGensIs r p

getGen :: Int -> [Rule] -> Pots -> Pots
getGen 0 _ p = p
getGen n r p = getGen (n-1) r (nextGen r p)

getGensIs :: [Rule] -> Pots -> [(Int, Int)]
getGensIs r p = getGensIs' 0 r p
  where
    getGensIs' n r p = (n,sumIs p) : getGensIs' (n+1) r (nextGen r p)

nextGen :: [Rule] -> Pots -> Pots
nextGen rs pots = Map.union toAdd $ Map.mapWithKey gen pots
  where
    toAdd :: Pots
    toAdd = Map.fromAscList $ zip [min-2,min-1,max+1,max+2] (repeat False)
    min = (fst . Map.findMin) pots
    max = (fst . Map.findMax) pots
    gen :: Int -> Bool -> Bool
    gen k _ = get5 k `elem` rs
    get5 :: Int -> [Bool]
    get5 k = [Map.findWithDefault False i pots | i <- [k-2..k+2]]

printPot :: Pots -> String
printPot = Map.elems . Map.map (\x -> if x then '#' else '.')

sumIs :: Pots -> Int
sumIs = Map.foldrWithKey (\k b s -> if b then s+k else s) 0