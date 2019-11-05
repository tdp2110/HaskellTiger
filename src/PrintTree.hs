module Main where

import qualified Parser
import qualified Semant
import qualified Translate

import System.Environment (getArgs)

parseToExp :: String -> Translate.Exp
parseToExp text =
  let
    parseResult = Parser.parse text
  in
    case parseResult of
      Left err -> error $ show err
      Right ast ->
        let
          semantResult = Semant.transProg ast
        in
          case semantResult of
            Left err -> error $ show err
            Right (Semant.ExpTy{Semant.exp=expr}, _, _, _) -> expr

main :: IO ()
main = do
  args <- getArgs
  str <- readFile $ head args
  case parseToExp str of
    Translate.Ex treeExp -> putStrLn $ show treeExp
    Translate.Nx treeStm -> putStrLn $ show treeStm
    Translate.Cx _ -> do putStrLn "CX ..."
