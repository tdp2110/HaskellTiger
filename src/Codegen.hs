module Codegen where

import qualified X64Frame
import qualified Symbol as S
import qualified Temp
import qualified Tree as Tree
import qualified Assem as A

import Control.Monad (forM_)
import Control.Monad.Trans.Writer (WriterT, tell)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.DList (DList, singleton)
import Data.Functor.Identity


type CodeGenerator = StateT Temp.Generator (
                       WriterT (DList A.Inst)
                         (ReaderT X64Frame.X64 Identity))


newTemp :: CodeGenerator Int
newTemp = do
  gen <- get
  let
    (t, gen') = Temp.newtemp gen
    in
    do
      put gen'
      pure t

emit :: A.Inst -> CodeGenerator ()
emit inst = do
  lift . tell $ singleton inst

getArch :: CodeGenerator X64Frame.X64
getArch = (lift . lift) ask

result :: (Int -> CodeGenerator [A.Inst]) -> CodeGenerator Int
result codegen = do
  t <- newTemp
  insts <- codegen t
  forM_
    insts
    (\inst -> emit inst)
  pure t

noop :: CodeGenerator ()
noop = do return ()

munchStm :: Tree.Stm -> CodeGenerator ()
munchStm (Tree.SEQ (a, b)) = do
  _ <- munchStm a
  munchStm b
munchStm (Tree.EXP (Tree.CONST _)) =
  noop
munchStm (Tree.EXP e) = do
   _ <- munchExp e
   noop
munchStm (Tree.LABEL l@(Temp.Label (S.Symbol s))) =
   emit $ A.LABEL { A.assem = s ++ ": "
                  , A.lab = l }
munchStm (Tree.MOVE (Tree.TEMP dst, Tree.TEMP src)) =
   emit $ A.MOVE { A.assem = "MOV `d0, `s0\n"
                 , A.moveDst = dst
                 , A.moveSrc = src }
munchStm (Tree.MOVE (Tree.MEM e1, e2)) = do
  src <- munchExp e2
  dst <- munchExp e1
  emit $ A.OPER { A.assem = "MOV [`d0], `s0\n"
                , A.operDst = [dst]
                , A.operSrc = [src]
                , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.TEMP t, e2)) = do
  src <- munchExp e2
  emit $ A.OPER { A.assem = "MOV `d0, `s0\n"
                , A.operDst = [t]
                , A.operSrc = [src]
                , A.jump = Nothing }
munchStm m@(Tree.MOVE _) = error $ "codegen can't handle move: " ++ show m
munchStm (Tree.JUMP (Tree.NAME lab, _)) =
  emit $ A.OPER { A.assem = "JMP `j0\n"
                , A.operDst = []
                , A.operSrc = []
                , A.jump = Just [lab] }
munchStm (Tree.JUMP (e, labels)) = do
  src <- munchExp e
  emit $ A.OPER { A.assem = "JMP `s0\n"
                , A.operSrc = [src]
                , A.operDst = []
                , A.jump = Just labels }
munchStm (Tree.CJUMP (op, e1, e2, t, f)) = do
  src1 <- munchExp e1
  src2 <- munchExp e2
  emit $ A.OPER { A.assem = "CMP `s1, `s2\n" ++ (opToJump op) ++ " `j0\nJMP `j1\n"
                , A.operDst = []
                , A.operSrc = [src1, src2]
                , A.jump = Just [t, f] }
  where
    opToJump :: Tree.Relop -> String
    opToJump oper = case oper of
                      Tree.EQ -> "JE"
                      Tree.NE -> "JNE"
                      Tree.LT -> "JL"
                      Tree.LE -> "JLE"
                      Tree.ULT -> "JL"
                      Tree.ULE -> "JLE"
                      Tree.GT -> "JG"
                      Tree.GE -> "JGE"
                      Tree.UGT -> "JG"
                      Tree.UGE -> "JGE"

munchExp :: Tree.Exp -> CodeGenerator Int
munchExp (Tree.CONST c) =
  result (\r -> let
                  inst = A.OPER { A.assem="MOV `d0, " ++ (show c) ++ "\n"
                                , A.operDst = [r]
                                , A.operSrc = []
                                , A.jump = Nothing }
                in
                  do
                    pure [inst]
        )
munchExp (Tree.TEMP t) = do
  pure t
munchExp (Tree.ESEQ (s, e)) = do
  _ <- munchStm s
  munchExp e
munchExp (Tree.BINOP (op, e1, e2)) =
  if op == Tree.DIV then
    result (\r -> do
                    src1 <- munchExp e1
                    src2 <- munchExp e2
                    x64 <- getArch
                    pure [ A.MOVE { A.assem="MOV `d0, `s0\n"
                                  , A.moveDst=X64Frame.dividendRegister x64
                                  , A.moveSrc=src1 }
                         , A.OPER { A.assem="IDIV `s0\n"
                                  , A.operDst=X64Frame.divideDests x64
                                  , A.operSrc=[src2]
                                  , A.jump=Nothing }
                         , A.MOVE { A.assem="MOV `d0, `s0\n"
                                  , A.moveDst=r
                                  , A.moveSrc=X64Frame.dividendRegister x64
                                  } ])
  else
    result (\r -> do
                    src1 <- munchExp e1
                    src2 <- munchExp e2
                    pure [ A.MOVE { A.assem="MOV `d0, `s0\n"
                                  , A.moveDst=r
                                  , A.moveSrc=src1 }
                         , A.OPER { A.assem=(convertOp op) ++ " `d0, `s0\n"
                                  , A.operDst=[r]
                                  , A.operSrc=[src2]
                                  , A.jump=Nothing } ])
  where
    convertOp :: Tree.Binop -> String
    convertOp oper = case oper of
                       Tree.PLUS -> "ADD"
                       Tree.MINUS -> "SUB"
                       Tree.MUL -> "IMUL"
                       Tree.DIV -> "IDIV"
                       Tree.AND -> "AND"
                       Tree.OR -> "OR"
                       Tree.LSHIFT -> "SHL"
                       Tree.RSHIFT -> "SHR"
                       Tree.XOR -> "XOR"
                       _ -> error $ "unsupported operator: " ++ (show oper)
