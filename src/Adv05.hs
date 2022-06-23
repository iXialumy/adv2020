module Adv05 where
import Data.List (sort)

input = readFile "resources/adv05.txt"

seatIdForLine :: String -> Int
seatIdForLine line = do
  let (row, seat, _, _) = foldr modifySeatID (127, 7, 128, 8) (reverse line)
  row * 8 + seat

modifySeatID :: Char -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
modifySeatID c (row, seat, remainingRow, remainingSeat)
  | c == 'F' = (row - div remainingRow 2, seat, div remainingRow 2, remainingSeat)
  | c == 'B' = (row, seat, div remainingRow 2, remainingSeat)
  | c == 'L' = (row, seat - div remainingSeat 2, remainingRow, div remainingSeat 2)
  | c == 'R' = (row, seat, remainingRow, div remainingSeat 2)
  | otherwise = (0, 0, 0, 0)

adv05 :: IO ()
adv05 = do
    adv05a
    adv05b

adv05a :: IO ()
adv05a = do
    l <- input
    print $ maximum $ map seatIdForLine $ lines l

adv05b :: IO ()
adv05b = do
    l <- input
    let sorted = sort $ map seatIdForLine $ lines l
    let pairs = zip sorted (tail sorted)
    print $ head $ map (\(x, y) -> div (x + y) 2) $ filter (\(x, y) -> y - x == 2) pairs