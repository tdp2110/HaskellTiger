{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMCodegen where

import qualified Absyn                         as A
import qualified Symbol                        as S

import qualified LLVM.AST                      as LL
import qualified LLVM.AST.Type                 as LL
import qualified LLVM.AST.IntegerPredicate     as LL

import qualified LLVM.IRBuilder.Module         as IRB
import qualified LLVM.IRBuilder.Monad          as IRB
import qualified LLVM.IRBuilder.Instruction    as IRB
import qualified LLVM.IRBuilder.Constant       as IRB

import           Data.Word
import           Data.ByteString.Short
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Data.Text                     as T
import           Data.Text                      ( Text )


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

newtype Env = Env { operands :: M.Map Text LL.Operand }
        deriving (Eq, Show)

registerOperand :: MonadState Env m => Text -> LL.Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

type LLVM = IRB.ModuleBuilderT (State Env)
type Codegen = IRB.IRBuilderT LLVM

zero :: LL.Operand
zero = IRB.int64 (0 :: Integer)

codegenExp :: A.Exp -> Codegen LL.Operand

codegenExp (A.IntExp i                    ) = pure $ IRB.int64 $ toInteger i

codegenExp (A.VarExp (A.SimpleVar sym pos)) = do
  operandEnv <- gets operands
  case M.lookup (S.name sym) operandEnv of
    Just op -> IRB.load op 8
    Nothing ->
      error $ "use of undefined variable " <> show sym <> " at " <> show pos

codegenExp (A.OpExp left oper right _) = do
  leftOperand  <- codegenExp left
  rightOperand <- codegenExp right
  let f = case oper of
        A.PlusOp   -> IRB.add
        A.MinusOp  -> IRB.sub
        A.TimesOp  -> IRB.mul
        A.DivideOp -> IRB.sdiv -- TODO need to check for division by zero
        A.EqOp     -> IRB.icmp LL.EQ
        A.NeqOp    -> IRB.icmp LL.NE
        A.LtOp     -> IRB.icmp LL.SLT
        A.LeOp     -> IRB.icmp LL.SLE
        A.GtOp     -> IRB.icmp LL.SGT
        A.GeOp     -> IRB.icmp LL.SGE
        A.ModOp    -> IRB.srem -- TODO need to check for division by zero
  f leftOperand rightOperand

codegenExp (A.AssignExp (A.SimpleVar var _) rhs pos) = do
  rhsOp      <- codegenExp rhs
  operandEnv <- gets operands
  case M.lookup (S.name var) operandEnv of
    Just lhsOp -> do
      IRB.store lhsOp 8 rhsOp
      pure zero
    Nothing ->
      error $ "use of undefined variable " <> show var <> " at " <> show pos

codegenExp (A.IfExp test then' (Just else') _) = mdo
  -- %entry
  ---------
  testOp <- codegenExp test
  test'  <- IRB.icmp LL.NE testOp zero
  IRB.condBr test' ifThen ifElse

  -- %if.then
  -----------
  ifThen   <- IRB.block `IRB.named` "if.then"
  ifThenOp <- codegenExp then'
  IRB.br ifExit

  -- %if.else
  -----------
  ifElse   <- IRB.block `IRB.named` "if.else"
  ifElseOp <- codegenExp else'
  IRB.br ifExit

  -- %if.exit
  -----------
  ifExit <- IRB.block `IRB.named` "if.exit"
  IRB.phi [(ifThenOp, ifThen), (ifElseOp, ifElse)]

codegenExp (A.SeqExp expAndPosns) = do
  exps <- forM expAndPosns (\(e, _) -> codegenExp e)
  case exps of
    [] -> pure zero
    _  -> pure $ last exps

codegenExp (A.CallExp funcSym args _) = do
  argOps <- forM args $ \arg -> do
    argOp <- codegenExp arg
    pure (argOp, [])
  operandEnv <- gets operands
  case M.lookup (S.name funcSym) operandEnv of
    Just funcOp -> IRB.call funcOp argOps
    Nothing     -> error $ "use of undeclared function " <> show funcSym

codegenExp (A.WhileExp test body _) = mdo
  IRB.br testLab

  testLab <- IRB.block `IRB.named` "while.test"
  testOp  <- codegenExp test
  test'   <- IRB.icmp LL.NE testOp zero
  IRB.condBr test' loopBody loopExit

  loopBody <- IRB.block `IRB.named` "while.body"
  _        <- codegenExp body
  IRB.br testLab

  loopExit <- IRB.block `IRB.named` "while.exit"
  pure zero

codegenExp (A.LetExp decs body _) = do
  forM_ decs codegenDecl
  codegenExp body

codegenExp e = error $ "unimplemented alternative in codegenExp: " <> show e

emptyModule :: String -> LL.Module
emptyModule label = LL.defaultModule { LL.moduleName = toShortBS label }

codegenTop :: A.Exp -> LLVM ()
codegenTop e =
  let nilPos = A.Pos { A.absChrOffset = -1, A.lineno = -1, A.colno = -1 }
      mainFn = A.FunDec
        { A.fundecName = S.Symbol "main"
        , A.params     = []
        , A.result     = Nothing
        , A.funBody    = A.SeqExp [(e, nilPos), (A.IntExp 0, nilPos)]
        , A.funPos     = nilPos
        }
  in  codegenFunDec mainFn

codegenDecl :: A.Dec -> Codegen ()
codegenDecl (A.FunctionDec [funDec]     ) = lift $ codegenFunDec funDec
codegenDecl (A.VarDec name _ _ initExp _) = do
  initOp <- codegenExp initExp
  addr   <- IRB.alloca LL.i64 Nothing 8
  IRB.store addr 8 initOp
  registerOperand (S.name name) addr
  pure ()
codegenDecl _ = undefined

codegenFunDec :: A.FunDec -> LLVM ()
codegenFunDec A.FunDec { A.fundecName = name, A.params = params, A.funBody = body }
  = mdo
    registerOperand (S.name name) fun
    fun <- IRB.function (LL.Name $ toShortBS $ show name) args LL.i64 genBody
    pure ()
 where
  args = toSig params

  toSig :: [A.Field] -> [(LL.Type, IRB.ParameterName)]
  toSig =
    fmap (\f -> (LL.i64, IRB.ParameterName $ toShortBS $ show $ A.fieldName f))

  genBody :: [LL.Operand] -> Codegen ()
  genBody ops = do
    _entry <- IRB.block `IRB.named` "entry"
    forM_ (zip ops params) $ \(op, field) -> do
      addr <- IRB.alloca LL.i64 Nothing 8
      IRB.store addr 8 op
      registerOperand (S.name $ A.fieldName field) addr
    codegenExp body >>= IRB.ret

newtype InternalName = InternalName String
newtype ExternalName = ExternalName String

builtins :: [(InternalName, ExternalName, [LL.Type], LL.Type)]
builtins =
  [ ( InternalName "print_int"
    , ExternalName "tiger_printintln"
    , [LL.i64]
    , LL.void
    )
  ]

emitBuiltin :: (InternalName, ExternalName, [LL.Type], LL.Type) -> LLVM ()
emitBuiltin (InternalName internalName, ExternalName externalName, argtys, retty)
  = do
    func <- IRB.extern (LL.mkName externalName) argtys retty
    registerOperand (T.pack internalName) func

codegenLLVM :: A.Exp -> LL.Module
codegenLLVM e =
  flip evalState (Env { operands = M.empty })
    $ IRB.buildModuleT "llvm-test"
    $ do
        mapM_ emitBuiltin builtins
        codegenTop e
