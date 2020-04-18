module Main where

import qualified Parser
import qualified Semant
import qualified Translate
import qualified X64Frame

import           System.Environment             ( getArgs )

parseToExp :: String -> (Translate.Exp, Semant.FragList)
parseToExp text =
  let parseResult = Parser.parse text
  in  case parseResult of
        Left err -> error $ show err
        Right ast ->
          let semantResult = Semant.transProg ast
          in  case semantResult of
                Left err -> error $ show err
                Right (Semant.ExpTy { Semant.exp = expr }, frags, _, _) ->
                  (expr, frags)

showFrag :: X64Frame.Frag -> IO ()
showFrag X64Frame.PROC { X64Frame.body = body } = do
  putStrLn ";; FRAG PROC:"
  print body
  putStrLn ";; END FRAG"
showFrag (X64Frame.STRING (lab, str)) = do
  putStrLn ";; FRAG STRING:\n"
  print (lab, str)
  putStrLn ";; END FRAG"

main :: IO ()
main = do
  args <- getArgs
  str  <- readFile $ head args
  case parseToExp str of
    (Translate.Ex treeExp, frags) -> do
      mapM_ showFrag frags
      print treeExp
      pure ()
    (Translate.Nx treeStm, frags) -> do
      mapM_ showFrag frags
      print treeStm
      pure ()
    (Translate.Cx _, frags) -> do
      mapM_ showFrag frags
      putStrLn "CX ..."
