module Main where

import qualified Assem
import qualified Canon
import qualified Parser
import qualified Semant
import qualified X64Frame

compileToAsm :: String -> String
compileToAsm text =
  let
    (Right ast) = Parser.parse text
    (Right (Semant.ExpTy{Semant.exp=expr}, frags, gen, x64)) = Semant.transProg ast
    --emit :: Semant.FragList ->
  in
    "asdf"

main :: IO ()
main = do
  return ()
