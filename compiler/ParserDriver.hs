module Main where

import System.Environment (getArgs)

import Parser

main :: IO()
main = do
  args <- getArgs
  str <- readFile $ head args
  print $ parse str
