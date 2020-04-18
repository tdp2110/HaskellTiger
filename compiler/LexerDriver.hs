module Main where

import           System.Environment             ( getArgs )

import           Lexer

main :: IO ()
main = do
  args <- getArgs
  case scanner $ head args of
    Left  st -> error st
    Right ls -> print ls
