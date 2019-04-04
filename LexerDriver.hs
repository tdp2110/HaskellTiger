module Main where

import Lexer

main :: IO()
main = do s <- getContents
          print (alexScanTokens s)
