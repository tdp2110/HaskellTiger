module Codegen (codegen) where

import qualified Assem as A
import qualified Frame
import qualified Symbol as S
import qualified Temp
import qualified TreeIR as TreeIR
import qualified X64Frame

import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.DList (DList, singleton, toList)
import Data.Functor.Identity
import Data.List
import qualified Data.Map as Map


codegen :: X64Frame.X64 -> Temp.Generator -> TreeIR.Stm -> ([A.Inst], Temp.Generator)
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
  let (t, gen') = Temp.newtemp gen
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

munchStm :: TreeIR.Stm -> CodeGenerator ()
munchStm (TreeIR.SEQ (a, b)) = do
  _ <- munchStm a
  munchStm b
munchStm (TreeIR.EXP (TreeIR.CONST _)) =
  noop
munchStm (TreeIR.EXP e) = do
   _ <- munchExp e
   noop
munchStm (TreeIR.LABEL l@(Temp.Label (S.Symbol s))) =
   emit A.LABEL { A.assem=s ++ ":"
                , A.lab = l }
munchStm (TreeIR.MOVE (TreeIR.TEMP dst, TreeIR.TEMP src)) =
   emit A.MOVE { A.assem="\tmov `d0, `s0"
               , A.moveDst = dst
               , A.moveSrc = src }
munchStm (TreeIR.MOVE (TreeIR.TEMP dst, TreeIR.CONST 0)) =
   emit A.OPER { A.assem="\txor `d0, `s0"
               , A.operDst=[dst]
               , A.operSrc=[dst]
               , A.jump=Nothing }
munchStm (TreeIR.MOVE (TreeIR.TEMP dst, TreeIR.CONST c)) =
   emit A.OPER { A.assem="\tmov `d0, " ++ (show c)
               , A.operDst = [dst]
               , A.operSrc = []
               , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP s, TreeIR.CONST c)))) =
  emit A.OPER { A.assem="\tmov `d0, qword ptr [`s0" ++ (plusMinusInt c) ++ "]"
              , A.operDst = [d]
              , A.operSrc = [s]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.CONST c, TreeIR.TEMP s)))) =
  munchStm (TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP s, TreeIR.CONST c))))
munchStm (TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP s0, TreeIR.TEMP s1)))) =
  emit A.OPER { A.assem="\tmov `d0, [`s0 + `s1]"
              , A.operDst = [d]
              , A.operSrc = [s0, s1]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.MINUS, TreeIR.TEMP s, TreeIR.CONST c)))) =
  munchStm $ TreeIR.MOVE (TreeIR.TEMP d, TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP s, TreeIR.CONST $ -c)))
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.TEMP d), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`s0], `s1"
              , A.operDst = []
              , A.operSrc = [d, eReg]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP d0, TreeIR.TEMP d1)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`s0+`s1], `s2"
              , A.operDst = []
              , A.operSrc = [d0, d1, eReg]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.CONST c1, TreeIR.TEMP d)), TreeIR.CONST c2)) =
  emit A.OPER { A.assem="\tmov qword ptr [`s0" ++ (plusMinusInt c1) ++ "], " ++ (show c2) ++ ""
              , A.operDst = []
              , A.operSrc = [d]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.CONST c, TreeIR.TEMP d)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`s0" ++ (plusMinusInt c) ++ "], `s1"
              , A.operDst = []
              , A.operSrc = [d, eReg]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP d, TreeIR.CONST c1)), TreeIR.CONST c2)) =
  emit A.OPER { A.assem="\tmov qword ptr [`s0" ++ (plusMinusInt c1) ++ "], " ++ (show c2) ++ ""
              , A.operDst = []
              , A.operSrc = [d]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP d, TreeIR.CONST c)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`s0" ++ (plusMinusInt c) ++ "], `s1"
              , A.operDst = []
              , A.operSrc = [d, eReg]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM (TreeIR.BINOP (TreeIR.MINUS, TreeIR.TEMP d, TreeIR.CONST c)), e)) = do
  eReg <- munchExp e
  emit A.OPER { A.assem="\tmov qword ptr [`s0" ++ (plusMinusInt $ -c) ++ "], `s1"
              , A.operDst = []
              , A.operSrc = [d, eReg]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.MEM e1, e2)) = do
  src <- munchExp e2
  dst <- munchExp e1
  emit A.OPER { A.assem="\tmov [`s0], `s1"
              , A.operDst = []
              , A.operSrc = [dst, src]
              , A.jump = Nothing }
munchStm (TreeIR.MOVE (TreeIR.TEMP t, e2)) = do
  src <- munchExp e2
  emit A.MOVE { A.assem="\tmov `d0, `s0"
              , A.moveDst = t
              , A.moveSrc = src }
munchStm m@(TreeIR.MOVE _) = error $ "codegen can't handle move: " ++ show m
munchStm (TreeIR.JUMP (TreeIR.NAME lab, _)) =
  emit A.OPER { A.assem="\tjmp `j0"
              , A.operDst = []
              , A.operSrc = []
              , A.jump = Just [lab] }
munchStm (TreeIR.JUMP (e, labels)) = do
  src <- munchExp e
  emit A.OPER { A.assem="\tjmp `s0"
              , A.operDst = []
              , A.operSrc = [src]
              , A.jump = Just labels }
munchStm (TreeIR.CJUMP (op, e1, e2, t, f)) = do
  src1 <- munchExp e1
  src2 <- munchExp e2
  emit A.OPER { A.assem="\tcmp `s0, `s1\n\t" ++ (opToJump op) ++ " `j0\n\tjmp `j1"
              , A.operDst = []
              , A.operSrc = [src1, src2]
              , A.jump = Just [t, f] }
  where
    opToJump :: TreeIR.Relop -> String
    opToJump oper = case oper of
                      TreeIR.EQ -> "je"
                      TreeIR.NE -> "jne"
                      TreeIR.LT -> "jl"
                      TreeIR.LE -> "jle"
                      TreeIR.ULT -> "jl"
                      TreeIR.ULE -> "jle"
                      TreeIR.GT -> "jg"
                      TreeIR.GE -> "jge"
                      TreeIR.UGT -> "jg"
                      TreeIR.UGE -> "jge"

munchExp :: TreeIR.Exp -> CodeGenerator Int
munchExp (TreeIR.CONST c) =
  result $ \r -> pure [ A.OPER { A.assem="\tmov `d0, " ++ (show c) ++ ""
                               , A.operDst = [r]
                               , A.operSrc = []
                               , A.jump = Nothing }]
munchExp (TreeIR.TEMP t) = do
  pure t
munchExp (TreeIR.ESEQ (s, e)) = do
  _ <- munchStm s
  munchExp e
munchExp (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP src1, TreeIR.TEMP src2)) =
  result $ \r -> pure [ A.OPER { A.assem="\t lea `d0, [`s0+`s1]"
                               , A.operDst=[r]
                               , A.operSrc=[src1, src2]
                               , A.jump=Nothing
                               }]
munchExp (TreeIR.BINOP (TreeIR.PLUS, TreeIR.TEMP s, TreeIR.CONST c)) =
  result $ \r -> pure [ A.OPER { A.assem="\t lea `d0, [`s0+" ++ (show c) ++ "]"
                               , A.operDst=[r]
                               , A.operSrc=[s]
                               , A.jump=Nothing
                               }]
munchExp (TreeIR.BINOP ( TreeIR.PLUS
                       , TreeIR.TEMP s0
                       , TreeIR.MEM(TreeIR.BINOP(TreeIR.PLUS, TreeIR.TEMP s1, TreeIR.CONST c)))) =
  result $ \r -> pure [ A.MOVE { A.assem="\tmov `d0, `s0"
                               , A.moveDst=r
                               , A.moveSrc=s0 }
                      , A.OPER { A.assem="\tadd `d0, [`s0+" ++ (show c) ++ "]"
                               , A.operDst=[r]
                               , A.operSrc=[s1, r]
                               , A.jump=Nothing }]
munchExp (TreeIR.BINOP ( TreeIR.PLUS
                       , TreeIR.TEMP s0
                       , TreeIR.MEM(TreeIR.BINOP(TreeIR.MINUS, TreeIR.TEMP s1, TreeIR.CONST c)))) =
  result $ \r -> pure [ A.MOVE { A.assem="\tmov `d0, `s0"
                               , A.moveDst=r
                               , A.moveSrc=s0 }
                      , A.OPER { A.assem="\tadd `d0, [`s0-" ++ (show c) ++ "]"
                               , A.operDst=[r]
                               , A.operSrc=[s1, r]
                               , A.jump=Nothing }]
munchExp (TreeIR.BINOP (TreeIR.PLUS, e1, e2)) =
  result $ \r -> do
                   src1 <- munchExp e1
                   src2 <- munchExp e2
                   pure [ A.OPER { A.assem="\t lea `d0, [`s0+`s1]"
                                 , A.operDst=[r]
                                 , A.operSrc=[src1, src2]
                                 , A.jump=Nothing } ]
munchExp (TreeIR.BINOP (op, e1, e2)) =
  if op == TreeIR.DIV then
    result $ \r -> do
                     src1 <- munchExp e1
                     src2 <- munchExp e2
                     x64 <- getArch
                     let divideDests = X64Frame.divideDests x64
                     pure [ A.MOVE { A.assem="\tmov `d0, `s0"
                                   , A.moveDst=X64Frame.dividendRegister x64
                                   , A.moveSrc=src1 }
                          , A.OPER { A.assem="\tcqo"
                                   , A.operDst=[X64Frame.rdx x64]
                                   , A.operSrc=[X64Frame.rax x64]
                                   , A.jump=Nothing }
                          , A.OPER { A.assem="\tidiv `s0"
                                   , A.operDst=divideDests
                                   , A.operSrc=src2:divideDests
                                   , A.jump=Nothing }
                          , A.MOVE { A.assem="\tmov `d0, `s0"
                                   , A.moveDst=r
                                   , A.moveSrc=X64Frame.dividendRegister x64
                                   } ]
  else
    result $ \r -> do
                     src1 <- munchExp e1
                     src2 <- munchExp e2
                     x64 <- getArch
                     let multiplyDests = X64Frame.multiplyDests x64
                     pure [ A.MOVE { A.assem="\tmov `d0, `s0"
                                   , A.moveDst=X64Frame.multiplicandRegister x64
                                   , A.moveSrc=src1 }
                          , A.OPER { A.assem="\t" ++ (convertOp op) ++ " `d0, `s0"
                                   , A.operDst=multiplyDests
                                   , A.operSrc=src2:multiplyDests
                                   , A.jump=Nothing }
                          , A.MOVE { A.assem="\tmov `d0, `s0"
                                   , A.moveDst=r
                                   , A.moveSrc=X64Frame.multiplicandRegister x64
                                   } ]
  where
    convertOp :: TreeIR.Binop -> String
    convertOp oper = case oper of
                       TreeIR.PLUS -> "add"
                       TreeIR.MINUS -> "sub"
                       TreeIR.MUL -> "imul"
                       TreeIR.DIV -> "idiv"
                       TreeIR.AND -> "and"
                       TreeIR.OR -> "or"
                       TreeIR.LSHIFT -> "shl"
                       TreeIR.RSHIFT -> "shr"
                       TreeIR.XOR -> "xor"
                       _ -> error $ "unsupported operator: " ++ (show oper)
munchExp (TreeIR.CALL (expr, args, escapes)) =
  result $ \r -> do
                   argRegs <- mapM munchExp args
                   exprReg <- munchExp expr
                   x64 <- getArch
                   doCall
                     (A.OPER { A.assem="\tcall `s0"
                             , A.operDst=X64Frame.callDests x64
                             , A.operSrc=exprReg:(X64Frame.rsp x64):(X64Frame.rbp x64):argRegs
                             , A.jump=Nothing })
                     (A.MOVE { A.assem="\tmov `d0, `s0"
                             , A.moveDst=r
                             , A.moveSrc=X64Frame.rax x64 })
                     argRegs
                     escapes
                     IsReturn
munchExp (TreeIR.CALLNORETURN (expr, args, escapes)) =
  result $ \r -> do
                   argRegs <- mapM munchExp args
                   exprReg <- munchExp expr
                   x64 <- getArch
                   doCall
                     (A.OPER { A.assem="\tcall `s0"
                             , A.operDst=X64Frame.callDests x64
                             , A.operSrc=exprReg:(X64Frame.rsp x64):argRegs
                             , A.jump=Nothing })
                     (A.MOVE { A.assem="\tmov `d0, `s0"
                             , A.moveDst=r
                             , A.moveSrc=X64Frame.rax x64 })
                     argRegs
                     escapes
                     NoReturn
munchExp (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, e, TreeIR.CONST c))) =
  result $ \r -> do
                   src <- munchExp e
                   pure [ A.OPER { A.assem="\tmov `d0, [`s0" ++ (plusMinusInt c) ++ "]"
                                 , A.operDst=[r]
                                 , A.operSrc=[src]
                                 , A.jump=Nothing } ]
munchExp (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, TreeIR.CONST c, e))) =
  munchExp (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, e, TreeIR.CONST c)))
munchExp (TreeIR.MEM (TreeIR.BINOP (TreeIR.MINUS, e, TreeIR.CONST c))) =
  munchExp (TreeIR.MEM (TreeIR.BINOP (TreeIR.PLUS, e, TreeIR.CONST $ -c)))
munchExp (TreeIR.MEM (TreeIR.CONST c)) =
  result $ \r -> pure [ A.OPER { A.assem="\tmov `d0, [" ++ (show c) ++ "]"
                               , A.operDst=[r]
                               , A.operSrc=[]
                               , A.jump=Nothing } ]
munchExp (TreeIR.MEM expr) =
  result $ \r -> do
                   exprReg <- munchExp expr
                   pure [ A.OPER { A.assem="\tmov `d0, [`s0]"
                                 , A.operDst=[r]
                                 , A.operSrc=[exprReg]
                                 , A.jump=Nothing } ]
munchExp (TreeIR.NAME (Temp.Label (S.Symbol s))) =
  result $ \r -> do
                   pure [ A.OPER { A.assem="\tlea `d0, [rip + " ++ s ++ "]"
                                 , A.operDst=[r]
                                 , A.operSrc=[]
                                 , A.jump=Nothing } ]

data ReturnsOrNot = IsReturn | NoReturn

doCall :: A.Inst -> A.Inst -> [Int] -> [Frame.EscapesOrNot] -> ReturnsOrNot
                             -> CodeGenerator [A.Inst]
doCall callStm moveStm callArgs escapes returnsOrNot = do
  x64 <- getArch
  let callerSaves = X64Frame.callerSaves x64
  callerSaveDests <- mapM (\_ -> newTemp) callerSaves
  let
    saves = fmap save $ zip callerSaves callerSaveDests
    restores = fmap restore $ zip callerSaves callerSaveDests
    (argMap, argSetup) = setupArgs x64 $ zip callArgs escapes
    callStm' = rewriteCall argMap callStm
    in
    case returnsOrNot of
      IsReturn ->
        pure $ saves ++ argSetup
                     ++ [callStm', moveStm]
                     ++ restores
      NoReturn ->
        pure $ argSetup ++ [callStm']
  where
    rewriteCall :: [(Int, Int)] -> A.Inst -> A.Inst
    rewriteCall argMap i@(A.OPER { A.operSrc=operSrc }) =
      let
        argMap' = Map.fromList argMap
        replaceElt ix = case Map.lookup ix argMap' of
                          Just ix' -> ix'
                          Nothing  -> ix
        operSrc' = fmap replaceElt operSrc
      in
        i { A.operSrc=operSrc' }
    rewriteCall _ i =
       error $ "shouldn't get here: illegal call instr " ++ (show i)

    save :: (Int, Int) -> A.Inst
    save (reg, temp) = A.MOVE { A.assem="\tmov `d0, `s0\t\t## caller saves"
                              , A.moveDst = temp
                              , A.moveSrc = reg }

    restore :: (Int, Int) -> A.Inst
    restore (reg, temp) = A.MOVE { A.assem="\tmov `d0, `s0\t\t## caller restores"
                                 , A.moveDst = reg
                                 , A.moveSrc = temp }

    setupArgs :: X64Frame.X64 -> [(Int, Frame.EscapesOrNot)] -> ([(Int, Int)], [A.Inst])
    setupArgs x64 paramsAndEscapes =
      let
        (_, paramMap, _, res) = foldl'
                        (step x64)
                        (X64Frame.paramRegs x64, [] :: [(Int, Int)], [0..], [] :: [A.Inst])
                        paramsAndEscapes
      in
        (paramMap, res)

    step :: X64Frame.X64 -> ([Int], [(Int, Int)], [Int], [A.Inst]) -> (Int, Frame.EscapesOrNot)
              -> ( [Int] -- remaining param regs
                 , [(Int, Int)] -- argReg, paramReg
                 , [Int] -- nextStackOffsets
                 , [A.Inst])
    step x64 (paramRegs, argMap, nextStackOffsets, acc) (argReg, escapesOrNot) =
      case escapesOrNot of
        Frame.Escapes ->
          let
            (stackOffset:nextStackOffsets') = nextStackOffsets
            inst = A.OPER { A.assem="\tmov qword ptr [`s0 - " ++ (show stackOffset) ++ "], `s1"
                          , A.operDst=[]
                          , A.operSrc=[X64Frame.rsp x64, argReg]
                          , A.jump=Nothing }
          in
            (paramRegs, argMap, nextStackOffsets', acc ++ [inst])
        Frame.DoesNotEscape ->
          case paramRegs of
            [] ->
              step x64 (paramRegs, argMap, nextStackOffsets, acc) (argReg, Frame.Escapes)
            (paramReg:paramRegs') ->
              ( paramRegs'
              , (argReg, paramReg):argMap
              , nextStackOffsets
              , acc ++ [ A.MOVE { A.assem="\tmov `d0, `s0"
                                , A.moveDst=paramReg
                                , A.moveSrc=argReg } ])
