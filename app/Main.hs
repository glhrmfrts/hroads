module Main where

import Level.IO
import Lib

main :: IO ()
main = do
  window <- initialize "Hoom"
  loadLevels "ROADS.LZS"
  mainLoop drawBlack window
  cleanup window
