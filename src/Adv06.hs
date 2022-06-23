module Adv06 where
import Data.List.Split (splitOn)
import Data.Char (isLetter)
import Data.Set (fromList, intersection)
import Data.List (sort, group)

adv06a = print . sum . map uniquePerGroup . splitOn "\n\n"

uniquePerGroup = length . fromList . filter isLetter

adv06b = print . sum . map commonPerGroup . splitOn "\n\n"

commonPerGroup = length . foldr1 intersection . map fromList . lines

adv06 = do
    input <- readFile "resources/adv06.txt"
    adv06a input
    adv06b input