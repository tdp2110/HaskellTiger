module Main where

import qualified Parser
import qualified Semant
import qualified Symbol
import qualified Temp
import qualified Translate
import qualified Tree as T

import System.Environment (getArgs)
import Prelude hiding (exp)

indent :: Int -> IO ()
indent 0 = do
  return ()
indent i = do
  putStr "  "
  indent $ i - 1

stm :: T.Stm -> Int -> IO ()
stm (T.SEQ (a,b)) d = do
  indent d
  putStrLn "SEQ("
  stm a $ d + 1
  putStrLn ","
  stm b $ d + 1
  putChar ')'
stm (T.LABEL (Temp.Label(Symbol.Symbol lab))) d = do
  indent d
  putStr "LABEL "
  putStr lab
stm (T.JUMP (e,_)) d = do
  indent d
  putStrLn "JUMP("
  exp e $ d + 1
  putChar ')'
stm (T.CJUMP (r,a,b,Temp.Label(Symbol.Symbol t),Temp.Label (Symbol.Symbol f))) d = do
  indent d
  putStr "CJUMP("
  relop r
  putStrLn ","
  exp a $ d + 1
  putStrLn ","
  exp b $ d + 1
  putStrLn ", "
  indent $ d + 1
  putStr t
  putChar ','
  putStr f
  putChar ')'
stm (T.MOVE (a,b)) d = do
  indent d
  putStrLn "MOVE"
  exp a $ d + 1
  putStrLn ","
  exp b $ d + 1
  putChar ')'
stm (T.EXP e) d = do
  indent d
  putStrLn "EXP("
  exp e $ d + 1
  putChar ')'

exp :: T.Exp -> Int -> IO ()
exp (T.BINOP (p,a,b)) d = do
  indent d
  putStr "BINOP("
  binop p
  putStrLn ","
  exp a $ d + 1
  putStrLn ","
  exp b $ d + 1
  putChar ')'
exp (T.MEM e) d = do
  indent d
  putStrLn "MEM("
  exp e $ d + 1
  putChar ')'
exp (T.TEMP t) d = do
  indent d
  putStr "TEMP "
  putStr $ show t
exp (T.ESEQ (s,e)) d = do
  indent d
  putStrLn "ESEQ("
  stm s $ d + 1
  putStrLn ","
  exp e $ d + 1
  putChar ')'
exp (T.NAME (Temp.Label (Symbol.Symbol lab))) d = do
  indent d
  putStr "NAME "
  putStr lab
exp (T.CONST i) d = do
  indent d
  putStr "CONST "
  putStr $ show i
exp (T.CALL (e,el)) d = do
  indent d
  putStrLn "CALL("
  exp e $ d + 1
  mapM_
    (\a -> do
              putStrLn ","
              exp a $ d + 2
    )
    el
  putStr ")"

binop :: T.Binop -> IO ()
binop T.PLUS = do putStr "PLUS"
binop T.MINUS = do putStr "MINUS"
binop T.MUL = do putStr "MUL"
binop T.DIV = do putStr "DIV"
binop T.AND = do putStr "AND"
binop T.OR = do putStr "OR"
binop T.LSHIFT = do putStr "LSHIFT"
binop T.RSHIFT = do putStr "RSHIFT"
binop T.ARSHIFT = do putStr "ARSHIFT"
binop T.XOR = do putStr "XOR"

relop :: T.Relop -> IO ()
relop T.EQ = do putStr "EQ"
relop T.NE = do putStr "NE"
relop T.LT = do putStr "LT"
relop T.GT = do putStr "GT"
relop T.LE = do putStr "LE"
relop T.GE = do putStr "GE"
relop T.ULT = do putStr "ULT"
relop T.ULE = do putStr "ULE"
relop T.UGT = do putStr "UGT"
relop T.UGE = do putStr "UGE"

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
            Right (Semant.ExpTy{Semant.exp=expr}, _) -> expr

main :: IO ()
main = do
  args <- getArgs
  str <- readFile $ head args
  case parseToExp str of
    Translate.Ex treeExp -> exp treeExp 0 >> putStrLn ""
    Translate.Nx treeStm -> stm treeStm 0 >> putStrLn ""
    Translate.Cx _ -> do putStrLn "CX ..."
