module TreeIR
  ( Exp(..)
  , Stm(..)
  , Binop(..)
  , Relop(..)
  , makeSeq
  , notRel
  , maxCallArgsStm
  )
where

import qualified Frame
import qualified Symbol
import qualified Temp

import           Control.Monad.Trans.Writer     ( Writer
                                                , tell
                                                , execWriter
                                                )
import           Data.DList                     ( DList
                                                , singleton
                                                , toList
                                                , fromList
                                                )

import           Prelude                 hiding ( GT
                                                , LT
                                                , EQ
                                                )


data Exp =
    CONST Int
  | NAME Temp.Label
  | TEMP Int
  | BINOP (Binop, Exp, Exp)
  | MEM Exp
  | CALL (Exp, [Exp], [Frame.EscapesOrNot],  {- is there a result?-} Bool)
  | CALLNORETURN (Exp, [Exp], [Frame.EscapesOrNot])
  | ESEQ (Stm, Exp)
  deriving (Eq)

data Stm =
    MOVE (Exp, Exp) -- (dst, src)
  | EXP Exp
  | JUMP (Exp, [Temp.Label])
  | CJUMP (Relop, Exp, Exp, Temp.Label, Temp.Label)
  | SEQ (Stm, Stm)
  | LABEL Temp.Label
  deriving (Eq)

makeSeq :: [TreeIR.Stm] -> TreeIR.Stm
makeSeq []             = TreeIR.EXP $ TreeIR.CONST 0
makeSeq (stmt : stmts) = TreeIR.SEQ (stmt, makeSeq stmts)

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
notRel op = case op of
  EQ  -> NE
  NE  -> EQ
  LT  -> GE
  GT  -> LE
  LE  -> GT
  GE  -> LT
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
  | i == 0 = pure ()
  | otherwise = do
    putStrW "  "
    indent $ i - 1


putStm :: Stm -> Int -> StmWriter
putStm (SEQ (a, b)) d = do
  indent d
  putStrLnW "SEQ("
  putStm a $ d + 1
  putStrLnW ","
  putStm b $ d + 1
  putCharW ')'
putStm (LABEL (Temp.Label (Symbol.Symbol lab))) d = do
  indent d
  putStrW "LABEL "
  putStrW lab
putStm (JUMP (e, _)) d = do
  indent d
  putStrLnW "JUMP("
  putExp e $ d + 1
  putCharW ')'
putStm (CJUMP (r, a, b, Temp.Label (Symbol.Symbol t), Temp.Label (Symbol.Symbol f))) d
  = do
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
putStm (MOVE (a, b)) d = do
  indent d
  putStrLnW "MOVE("
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
putExp (BINOP (p, a, b)) d = do
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
putExp (ESEQ (s, e)) d = do
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
putExp (CALL (e, el, _, _)) d = do
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
putExp (CALLNORETURN (e, el, _)) d = do
  indent d
  putStrLnW "CALLNORETURN("
  putExp e $ d + 1
  mapM_
    (\a -> do
      putStrLnW ","
      putExp a $ d + 1
    )
    el
  putStrW ")"

binop :: Binop -> StmWriter
binop PLUS    = putStrW "PLUS"
binop MINUS   = putStrW "MINUS"
binop MUL     = putStrW "MUL"
binop DIV     = putStrW "DIV"
binop AND     = putStrW "AND"
binop OR      = putStrW "OR"
binop LSHIFT  = putStrW "LSHIFT"
binop RSHIFT  = putStrW "RSHIFT"
binop ARSHIFT = putStrW "ARSHIFT"
binop XOR     = putStrW "XOR"

relop :: Relop -> StmWriter
relop EQ  = putStrW "EQ"
relop NE  = putStrW "NE"
relop LT  = putStrW "LT"
relop GT  = putStrW "GT"
relop LE  = putStrW "LE"
relop GE  = putStrW "GE"
relop ULT = putStrW "ULT"
relop ULE = putStrW "ULE"
relop UGT = putStrW "UGT"
relop UGE = putStrW "UGE"

maxCallArgsStm :: Stm -> Maybe Int
maxCallArgsStm (MOVE (e1, e2)) =
  nullableMax (maxCallArgsExp e1) (maxCallArgsExp e2)
maxCallArgsStm (EXP  e     ) = maxCallArgsExp e
maxCallArgsStm (JUMP (e, _)) = maxCallArgsExp e
maxCallArgsStm (CJUMP (_, e1, e2, _, _)) =
  nullableMax (maxCallArgsExp e1) (maxCallArgsExp e2)
maxCallArgsStm (SEQ   (s1, s2)) = max (maxCallArgsStm s1) (maxCallArgsStm s2)
maxCallArgsStm (LABEL _       ) = Nothing

maxCallArgsExp :: Exp -> Maybe Int
maxCallArgsExp (CONST _) = Nothing
maxCallArgsExp (NAME  _) = Nothing
maxCallArgsExp (TEMP  _) = Nothing
maxCallArgsExp (BINOP (_, e1, e2)) =
  nullableMax (maxCallArgsExp e1) (maxCallArgsExp e2)
maxCallArgsExp (MEM  e                    ) = maxCallArgsExp e
maxCallArgsExp (CALL (funcExp, args, _, _)) = nullableMaximum
  [ maxCallArgsExp funcExp
  , nullableMaximum $ fmap maxCallArgsExp args
  , Just $ fromIntegral $ length args
  ]
maxCallArgsExp (CALLNORETURN (funcExp, args, esc)) =
  maxCallArgsExp (CALL (funcExp, args, esc, False))
maxCallArgsExp (ESEQ (s, e)) =
  nullableMax (maxCallArgsStm s) (maxCallArgsExp e)

nullableMax :: Maybe Int -> Maybe Int -> Maybe Int
nullableMax (  Just x) (Just y)   = Just $ max x y
nullableMax j@(Just _) Nothing    = j
nullableMax Nothing    j@(Just _) = j
nullableMax _          _          = Nothing

nullableMaximum :: [Maybe Int] -> Maybe Int
nullableMaximum = foldr nullableMax Nothing
