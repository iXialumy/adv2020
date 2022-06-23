module Adv07 where

import Data.List (intersect)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace, traceId, traceShow, traceShowId, traceStack)

parseLine :: String -> (String, [(Int, String)])
parseLine s = (parseColor main, rest)
  where
    split = map trim $ splitOn "contain" s
    main = head split
    rest = mapMaybe parseCounts $ splitOn "," $ head $ tail split

parseCounts "no other bags." = Nothing
parseCounts bag = Just (count, color)
  where
    w = words bag
    count = read $ head w
    color = unwords $ cutlast $ tail w

parseColor bag = unwords $ cutlast $ words bag

cutlast [] = []
cutlast [x] = []
cutlast (x : xs) = x : cutlast xs

anyContainsAnyOf :: Map.Map String [(Int, String)] -> Set.Set String -> Set.Set String
anyContainsAnyOf rules bags =
  Set.union bags $
    Set.fromList $
      filter (\key -> containsAnyOf bags $ rules Map.! key) $
        filter (`notElem` bags) $
          Map.keys rules

containsAnyOf colors bags = not $ null $ Set.intersection (Set.fromList $ map snd bags) colors

converge :: Ord a => [Set.Set a] -> Set.Set a
converge [] = Set.fromList []
converge [x] = x
converge (x : ys@(y : _))
  | x == y = x
  | otherwise = converge ys

adv07a input =
  print $ length $ flip Set.difference sg $ converge $ iterate (anyContainsAnyOf rules) sg
  where
    rules = Map.fromList $ map parseLine $ lines input
    sg = Set.singleton "shiny gold"

adv07b input = print $ subtract 1 $ recursiveCounts rules "shiny gold"
  where
    rules = Map.fromList $ map parseLine $ lines input

recursiveCounts :: Map.Map String [(Int, String)] -> String -> Int
recursiveCounts rules color
  | null (rules ! color) = 1
  | otherwise = (1 +) $ sum $ map (\(count, color) -> count * recursiveCounts rules color) $ rules ! color

adv07 = do
  input <- readFile "resources/adv07.txt"
  adv07a input
  adv07b input