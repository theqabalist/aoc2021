module Main where

import Data.Text.IO (putStr, readFile)
import Days
import System.Environment (getArgs)
import Prelude hiding (putStr, readFile)

main :: IO ()
main = do
  args <- getArgs
  let day = read $ head args
  let program = days !! (day - 1)
  input <- readFile $ "inputs/" <> show day <> ".txt"
  putStr $ program input