{-# LANGUAGE OverloadedStrings #-}

module TreeIR
  ( Exp(..)
  , Stm(..)
  , Binop(..)
  , Relop(..)
  , makeSeq
  , notRel
  , maxCallArgsAndEscapesStm
  , fmtDebug
  )
where

import qualified Absyn
import qualified Frame
import qualified Symbol
import qualified Temp

import           Control.Monad                  ( when )
import           Control.Monad.Trans.Writer     ( Writer
                                                , tell
                                                , execWriter
                                                )
import           Data.DList                     ( DList
                                                , singleton
                                                , toList
                                                , fromList
                                                )
import qualified Data.Text                     as T
import           Data.Maybe                     ( isJust )

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

type DebugInfo = Maybe (Symbol.Symbol, Absyn.Pos)

data Stm =
    MOVE (Exp, Exp) -- (dst, src)
  | EXP Exp
  | JUMP (Exp, [Temp.Label])
  | CJUMP (Relop, Exp, Exp, Temp.Label, Temp.Label)
  | SEQ (Stm, Stm)
  | LABEL (Temp.Label, DebugInfo)
  deriving (Eq)

fmtDebug :: DebugInfo -> String
fmtDebug (Just info) = show info
fmtDebug Nothing     = ""

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

putStrW :: T.Text -> StmWriter
putStrW s = do
  tell $ fromList $ T.unpack s
  pure ()

putStrLnW :: T.Text -> StmWriter
putStrLnW s = putStrW $ s `mappend` "\n"

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
putStm (LABEL (Temp.Label (Symbol.Symbol lab), info)) d = do
  indent d
  putStrW "LABEL "
  putStrW lab
  when (isJust info) $ do
    putStrW $ T.pack $ " ## " ++ fmtDebug info
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
  putStrW . T.pack $ show t
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
  putStrW . T.pack $ show i
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

-- | Max call args and escaping params to any functions in the stm
maxCallArgsAndEscapesStm :: Stm -> Maybe (Int, Int)
maxCallArgsAndEscapesStm (MOVE (e1, e2)) =
  nullableMax (maxCallArgsAndEscapesExp e1) (maxCallArgsAndEscapesExp e2)
maxCallArgsAndEscapesStm (EXP  e     ) = maxCallArgsAndEscapesExp e
maxCallArgsAndEscapesStm (JUMP (e, _)) = maxCallArgsAndEscapesExp e
maxCallArgsAndEscapesStm (CJUMP (_, e1, e2, _, _)) =
  nullableMax (maxCallArgsAndEscapesExp e1) (maxCallArgsAndEscapesExp e2)
maxCallArgsAndEscapesStm (SEQ   (s1, s2)) = nullableMax (maxCallArgsAndEscapesStm s1) (maxCallArgsAndEscapesStm s2)
maxCallArgsAndEscapesStm (LABEL _       ) = Nothing

maxCallArgsAndEscapesExp :: Exp -> Maybe (Int, Int)
maxCallArgsAndEscapesExp (CONST _) = Nothing
maxCallArgsAndEscapesExp (NAME  _) = Nothing
maxCallArgsAndEscapesExp (TEMP  _) = Nothing
maxCallArgsAndEscapesExp (BINOP (_, e1, e2)) =
  nullableMax (maxCallArgsAndEscapesExp e1) (maxCallArgsAndEscapesExp e2)
maxCallArgsAndEscapesExp (MEM  e                    ) = maxCallArgsAndEscapesExp e
maxCallArgsAndEscapesExp (CALL (funcExp, args, escapes, _)) = nullableMaximum
  [ maxCallArgsAndEscapesExp funcExp
  , nullableMaximum $ fmap maxCallArgsAndEscapesExp args
  , Just (fromIntegral $ length args, fromIntegral $ length $ filter isEscape escapes)
  ]
maxCallArgsAndEscapesExp (CALLNORETURN (funcExp, args, esc)) =
  maxCallArgsAndEscapesExp (CALL (funcExp, args, esc, False))
maxCallArgsAndEscapesExp (ESEQ (s, e)) =
  nullableMax (maxCallArgsAndEscapesStm s) (maxCallArgsAndEscapesExp e)

nullableMax :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
nullableMax (  Just (x1, x2)) (Just (y1, y2))   = Just (max x1 y1, max x2 y2)
nullableMax j@(Just _) Nothing    = j
nullableMax Nothing    j@(Just _) = j
nullableMax _          _          = Nothing

nullableMaximum :: [Maybe (Int, Int)] -> Maybe (Int, Int)
nullableMaximum = foldr nullableMax Nothing

isEscape :: Frame.EscapesOrNot -> Bool
isEscape Frame.Escapes = True
isEscape Frame.DoesNotEscape = False
