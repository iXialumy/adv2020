module Main where

import Adv01 (adv01)
import Adv02 (adv02)
import Adv03 (adv03)
import Adv04 (adv04)
import Adv05 (adv05)
import Adv06 (adv06)
import Adv07 (adv07)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let n = head args
  run n

run :: String -> IO ()
run n = case n of
  "01" -> adv01
  "02" -> adv02
  "03" -> adv03
  "04" -> adv04
  "05" -> adv05
  "06" -> adv06
  "07" -> adv07
  _ -> print "Use a valid day number!"