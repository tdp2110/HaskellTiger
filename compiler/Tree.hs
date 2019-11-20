module Tree where

import qualified Frame
import qualified Symbol
import qualified Temp

import Control.Monad.Trans.Writer (Writer, tell, execWriter)
import Data.DList (DList, singleton, toList, fromList)

import Prelude hiding (GT, LT, EQ)


data Exp =
    CONST Int
  | NAME Temp.Label
  | TEMP Int
  | BINOP (Binop, Exp, Exp)
  | MEM Exp
  | CALL (Exp, [Exp], [Frame.EscapesOrNot])
  | ESEQ (Stm, Exp)
  deriving (Eq)

data Stm =
    MOVE (Exp, Exp)
  | EXP Exp
  | JUMP (Exp, [Temp.Label])
  | CJUMP (Relop, Exp, Exp, Temp.Label, Temp.Label)
  | SEQ (Stm, Stm)
  | LABEL Temp.Label
  deriving (Eq)

makeSeq :: [Tree.Stm] -> Tree.Stm
makeSeq [] = Tree.EXP $ Tree.CONST 0
makeSeq (stmt:stmts) = Tree.SEQ(stmt, makeSeq stmts)

instance Show Stm where
  show stm = toList $ execWriter $ putStm stm 0

instance Show Exp where
  show expr = toList $ execWriter $ putExp expr 0

data Binop =
    PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | XOR
  deriving (Eq, Show)

data  Relop =
    EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | ULT
  | ULE
  | UGT
  | UGE
  deriving (Eq, Show)

notRel :: Relop -> Relop
notRel op =
  case op of
    EQ -> NE
    NE -> EQ
    LT -> GE
    GT -> LE
    LE -> GT
    GE -> LT
    ULT -> UGE
    UGT -> ULE
    ULE -> UGT
    UGE -> ULT

type StmWriter = Writer (DList Char) ()

putStrW :: String -> StmWriter
putStrW s = do
              tell $ fromList s
              pure ()

putStrLnW :: String -> StmWriter
putStrLnW s = putStrW $ s ++ "\n"

putCharW :: Char -> StmWriter
putCharW c = tell $ singleton c

indent :: Int -> StmWriter
indent i
  | i == 0    = do pure ()
  | otherwise = do
                  putStrW "  "
                  indent $ i - 1


putStm :: Stm -> Int -> StmWriter
putStm (SEQ (a,b)) d = do
  indent d
  putStrLnW "SEQ("
  putStm a $ d + 1
  putStrLnW ","
  putStm b $ d + 1
  putCharW ')'
putStm (LABEL (Temp.Label(Symbol.Symbol lab))) d = do
  indent d
  putStrW "LABEL "
  putStrW lab
putStm (JUMP (e,_)) d = do
  indent d
  putStrLnW "JUMP("
  putExp e $ d + 1
  putCharW ')'
putStm (CJUMP (r,a,b,Temp.Label(Symbol.Symbol t),Temp.Label (Symbol.Symbol f))) d = do
  indent d
  putStrW "CJUMP("
  relop r
  putStrLnW ","
  putExp a $ d + 1
  putStrLnW ","
  putExp b $ d + 1
  putStrLnW ", "
  indent $ d + 1
  putStrW t
  putCharW ','
  putStrW f
  putCharW ')'
putStm (MOVE (a,b)) d = do
  indent d
  putStrLnW "MOVE"
  putExp a $ d + 1
  putStrLnW ","
  putExp b $ d + 1
  putCharW ')'
putStm (EXP e) d = do
  indent d
  putStrLnW "EXP("
  putExp e $ d + 1
  putCharW ')'

putExp :: Exp -> Int -> StmWriter
putExp (BINOP (p,a,b)) d = do
  indent d
  putStrW "BINOP("
  binop p
  putStrLnW ","
  putExp a $ d + 1
  putStrLnW ","
  putExp b $ d + 1
  putCharW ')'
putExp (MEM e) d = do
  indent d
  putStrLnW "MEM("
  putExp e $ d + 1
  putCharW ')'
putExp (TEMP t) d = do
  indent d
  putStrW "TEMP "
  putStrW $ show t
putExp (ESEQ (s,e)) d = do
  indent d
  putStrLnW "ESEQ("
  putStm s $ d + 1
  putStrLnW ","
  putExp e $ d + 1
  putCharW ')'
putExp (NAME (Temp.Label (Symbol.Symbol lab))) d = do
  indent d
  putStrW "NAME "
  putStrW lab
putExp (CONST i) d = do
  indent d
  putStrW "CONST "
  putStrW $ show i
putExp (CALL (e,el,_)) d = do
  indent d
  putStrLnW "CALL("
  putExp e $ d + 1
  mapM_
    (\a -> do
             putStrLnW ","
             putExp a $ d + 1
    )
    el
  putStrW ")"

binop :: Binop -> StmWriter
binop PLUS = do putStrW "PLUS"
binop MINUS = do putStrW "MINUS"
binop MUL = do putStrW "MUL"
binop DIV = do putStrW "DIV"
binop AND = do putStrW "AND"
binop OR = do putStrW "OR"
binop LSHIFT = do putStrW "LSHIFT"
binop RSHIFT = do putStrW "RSHIFT"
binop ARSHIFT = do putStrW "ARSHIFT"
binop XOR = do putStrW "XOR"

relop :: Relop -> StmWriter
relop EQ = do putStrW "EQ"
relop NE = do putStrW "NE"
relop LT = do putStrW "LT"
relop GT = do putStrW "GT"
relop LE = do putStrW "LE"
relop GE = do putStrW "GE"
relop ULT = do putStrW "ULT"
relop ULE = do putStrW "ULE"
relop UGT = do putStrW "UGT"
relop UGE = do putStrW "UGE"
