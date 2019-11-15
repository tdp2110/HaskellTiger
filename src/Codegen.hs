module Codegen (codegen) where

import qualified Assem as A
import qualified Frame
import qualified Symbol as S
import qualified Temp
import qualified Tree as Tree
import qualified X64Frame

import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.DList (DList, singleton, toList)
import Data.Functor.Identity
import Data.List


codegen :: X64Frame.X64 -> Temp.Generator -> Tree.Stm -> ([A.Inst], Temp.Generator)
codegen x64 gen stm =
  let
    ((_, gen'), instsDList) =
      runIdentity . flip runReaderT x64 . runWriterT . flip runStateT gen $ munchStm stm
  in
    (toList instsDList, gen')

type CodeGenerator = StateT Temp.Generator (
                       WriterT (DList A.Inst)
                         (ReaderT X64Frame.X64 Identity))


plusMinusInt :: Int -> String
plusMinusInt i
  | i < 0     = show i
  | otherwise = "+" ++ show i

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
result codeGenerator = do
  t <- newTemp
  insts <- codeGenerator t
  mapM_ emit insts
  pure t

noop :: CodeGenerator ()
noop = do pure ()

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
   emit A.LABEL { A.assem=s ++ ":\n"
                , A.lab = l }
munchStm (Tree.MOVE (Tree.TEMP dst, Tree.TEMP src)) =
   emit A.MOVE { A.assem="\tmov `d0, `s0\n"
               , A.moveDst = dst
               , A.moveSrc = src }
munchStm (Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP s, Tree.CONST c)))) =
  emit A.MOVE { A.assem="\tmov `d0, [`s0" ++ (plusMinusInt c) ++ "]\n"
              , A.moveDst = d
              , A.moveSrc = s }
munchStm (Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST c, Tree.TEMP s)))) =
  munchStm (Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP s, Tree.CONST c))))
munchStm (Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP s0, Tree.TEMP s1)))) =
  emit A.OPER { A.assem="\tmov `d0, [`s0 + `s1]\n"
              , A.operDst = [d]
              , A.operSrc = [s0, s1]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.MINUS, Tree.TEMP s, Tree.CONST c)))) =
  munchStm $ Tree.MOVE (Tree.TEMP d, Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP s, Tree.CONST $ -c)))
munchStm (Tree.MOVE (Tree.MEM (Tree.TEMP d), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`d0], `s0\n"
              , A.operDst = [d]
              , A.operSrc = [eReg]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP d0, Tree.TEMP d1)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`d0+`d1], `s0\n"
              , A.operDst = [d0, d1]
              , A.operSrc = [eReg]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST c1, Tree.TEMP d)), Tree.CONST c2)) =
  emit A.OPER { A.assem="\tmov qword ptr [`d0" ++ (plusMinusInt c1) ++ "], " ++ (show c2) ++ "\n"
              , A.operDst = [d]
              , A.operSrc = []
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST c, Tree.TEMP d)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`d0" ++ (plusMinusInt c) ++ "], `s0\n"
              , A.operDst = [d]
              , A.operSrc = [eReg]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP d, Tree.CONST c1)), Tree.CONST c2)) =
  emit A.OPER { A.assem="\tmov qword ptr [`d0" ++ (plusMinusInt c1) ++ "], " ++ (show c2) ++ "\n"
              , A.operDst = [d]
              , A.operSrc = []
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP d, Tree.CONST c)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`d0" ++ (plusMinusInt c) ++ "], `s0\n"
              , A.operDst = [d]
              , A.operSrc = [eReg]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.MINUS, Tree.TEMP d, Tree.CONST c)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`d0" ++ (plusMinusInt $ -c) ++ "], `s0\n"
              , A.operDst = [d]
              , A.operSrc = [eReg]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.MEM e1, e2)) = do
  src <- munchExp e2
  dst <- munchExp e1
  emit A.OPER { A.assem="\tmov [`d0], `s0\n"
              , A.operDst = [dst]
              , A.operSrc = [src]
              , A.jump = Nothing }
munchStm (Tree.MOVE (Tree.TEMP t, e2)) = do
  src <- munchExp e2
  emit A.OPER { A.assem="\tmov `d0, `s0\n"
              , A.operDst = [t]
              , A.operSrc = [src]
              , A.jump = Nothing }
munchStm m@(Tree.MOVE _) = error $ "codegen can't handle move: " ++ show m
munchStm (Tree.JUMP (Tree.NAME lab, _)) =
  emit A.OPER { A.assem="\tJMP `j0\n"
              , A.operDst = []
              , A.operSrc = []
              , A.jump = Just [lab] }
munchStm (Tree.JUMP (e, labels)) = do
  src <- munchExp e
  emit A.OPER { A.assem="\tjmp `s0\n"
              , A.operSrc = [src]
              , A.operDst = []
              , A.jump = Just labels }
munchStm (Tree.CJUMP (op, e1, e2, t, f)) = do
  src1 <- munchExp e1
  src2 <- munchExp e2
  emit A.OPER { A.assem="\tcmp `s1, `s2\n" ++ (opToJump op) ++ " `j0\nJMP `j1\n"
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
                  inst = A.OPER { A.assem="\tmov `d0, " ++ (show c) ++ "\n"
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
                    pure [ A.MOVE { A.assem="\tmov `d0, `s0\n"
                                  , A.moveDst=X64Frame.dividendRegister x64
                                  , A.moveSrc=src1 }
                         , A.OPER { A.assem="\tidiv `s0\n"
                                  , A.operDst=X64Frame.divideDests x64
                                  , A.operSrc=[src2]
                                  , A.jump=Nothing }
                         , A.MOVE { A.assem="mov `d0, `s0\n"
                                  , A.moveDst=r
                                  , A.moveSrc=X64Frame.dividendRegister x64
                                  } ])
  else
    result (\r -> do
                    src1 <- munchExp e1
                    src2 <- munchExp e2
                    pure [ A.MOVE { A.assem="\tmov `d0, `s0\n"
                                  , A.moveDst=r
                                  , A.moveSrc=src1 }
                         , A.OPER { A.assem="\t" ++ (convertOp op) ++ " `d0, `s0\n"
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
munchExp (Tree.CALL (Tree.NAME lab, args, escapes)) =
  result (\r -> do
                  operSrc <- mapM munchExp args
                  x64 <- getArch
                  saveRestoreAndSetupArgs
                    (A.OPER { A.assem="\tcall `j0\n"
                            , A.operDst=X64Frame.callDests x64
                            , A.operSrc=operSrc
                            , A.jump=Just [lab] })
                    (A.MOVE { A.assem="\tmov `d0, `s0\n"
                            , A.moveDst=r
                            , A.moveSrc=X64Frame.rax x64 })
                    operSrc
                    escapes
         )
munchExp (Tree.CALL (expr, args, escapes)) =
  result (\r -> do
                  argRegs <- mapM munchExp args
                  exprReg <- munchExp expr
                  x64 <- getArch
                  saveRestoreAndSetupArgs
                    (A.OPER { A.assem="\tcall `s0\n"
                           , A.operDst=X64Frame.callDests x64
                           , A.operSrc=[exprReg] ++ argRegs
                           , A.jump=Nothing })
                    (A.MOVE { A.assem="\tmov `d0, `s0\n"
                           , A.moveDst=r
                           , A.moveSrc=X64Frame.rax x64 })
                    argRegs
                    escapes
         )
munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST c))) =
  result (\r -> do
                  src <- munchExp e
                  pure [ A.OPER { A.assem="\tmov `d0, [`s0" ++ (plusMinusInt c) ++ "]\n"
                                , A.operDst=[r]
                                , A.operSrc=[src]
                                , A.jump=Nothing } ]
         )
munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST c, e))) =
  munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST c)))
munchExp (Tree.MEM (Tree.BINOP (Tree.MINUS, e, Tree.CONST c))) =
  munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, e, Tree.CONST $ -c)))
munchExp (Tree.MEM (Tree.CONST c)) =
  result (\r -> do
                  pure [ A.OPER { A.assem="\tmov `d0, [" ++ (show c) ++ "]\n"
                                , A.operDst=[r]
                                , A.operSrc=[]
                                , A.jump=Nothing } ]
         )
munchExp (Tree.MEM expr) =
  result (\r -> do
                  exprReg <- munchExp expr
                  pure [ A.OPER { A.assem="\tmov `d0, [`s0]\n"
                                , A.operDst=[r]
                                , A.operSrc=[exprReg]
                                , A.jump=Nothing } ]
         )
munchExp (Tree.NAME lab@(Temp.Label (S.Symbol s))) =
  result (\r -> do
                  pure [ A.OPER { A.assem="\tlea `d0, [" ++ s ++ "]"
                                , A.operDst=[r]
                                , A.operSrc=[]
                                , A.jump=Just [lab] } ]
         )

saveRestoreAndSetupArgs :: A.Inst -> A.Inst -> [Int] -> [Frame.EscapesOrNot]
                             -> CodeGenerator [A.Inst]
saveRestoreAndSetupArgs callStm moveStm callArgs escapes = do
  x64 <- getArch
  let
    callerSaves = X64Frame.callerSaves x64
    in
    do
      callerSaveDests <- mapM (\_ -> newTemp) callerSaves
      let
        saves = fmap save $ zip callerSaves callerSaveDests
        restores = fmap restore $ zip callerSaves callerSaveDests
        in
        pure $ saves ++ (setupArgs x64 $ zip callArgs escapes)
                     ++ [callStm, moveStm]
                     ++ restores
  where
    save :: (Int, Int) -> A.Inst
    save (reg, temp) = A.MOVE { A.assem="\tmov `d0, `s0\n"
                              , A.moveDst = temp
                              , A.moveSrc = reg }

    restore :: (Int, Int) -> A.Inst
    restore (reg, temp) = save (temp, reg)

    setupArgs :: X64Frame.X64 -> [(Int, Frame.EscapesOrNot)] -> [A.Inst]
    setupArgs x64 paramsAndEscapes =
      let
        (_, _, res) = foldl'
                        (step x64)
                        (X64Frame.paramRegs x64, [0..], [] :: [A.Inst])
                        paramsAndEscapes
      in
        res

    step :: X64Frame.X64 -> ([Int], [Int], [A.Inst]) -> (Int, Frame.EscapesOrNot)
              -> ([Int], [Int], [A.Inst])
    step x64 (paramRegs, nextStackOffsets, acc) (argReg, escapesOrNot) =
      case escapesOrNot of
        Frame.Escapes ->
          let
            (stackOffset:nextStackOffsets') = nextStackOffsets
            inst = A.OPER { A.assem="\tmov qword ptr [`d0 + " ++ (show stackOffset) ++ "], `s0\n"
                          , A.operDst=[X64Frame.rsp x64]
                          , A.operSrc=[argReg]
                          , A.jump=Nothing }
          in
            (paramRegs, nextStackOffsets', acc ++ [inst])
        Frame.NoEscape ->
          case paramRegs of
            [] ->
              step x64 (paramRegs, nextStackOffsets, acc) (argReg, Frame.Escapes)
            (paramReg:paramRegs') ->
              ( paramRegs'
              , nextStackOffsets
              , acc ++ [ A.MOVE { A.assem="\tmov `d0, `s0\n"
                                , A.moveDst=paramReg
                                , A.moveSrc=argReg } ])
