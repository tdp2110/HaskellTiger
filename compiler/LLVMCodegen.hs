{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMCodegen where

import qualified Absyn                         as A
import qualified Symbol                        as S

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Type                 as AST
import qualified LLVM.AST.IntegerPredicate     as AST

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L

import           Data.Word
import           Data.ByteString.Short
import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.Text                      ( Text )


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

newtype Env = Env { operands :: M.Map Text AST.Operand }
        deriving (Eq, Show)

registerOperand :: MonadState Env m => Text -> AST.Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

zero :: AST.Operand
zero = L.int64 $ (0 :: Integer)

codegenExp :: A.Exp -> Codegen AST.Operand
codegenExp A.NilExp                         = pure zero
codegenExp (A.IntExp i                    ) = pure $ L.int64 $ toInteger i
codegenExp (A.VarExp (A.SimpleVar sym pos)) = do
  operandEnv <- gets operands
  case M.lookup (S.name sym) operandEnv of
    Just op -> L.load op 8
    Nothing ->
      error $ "use of undefined variable " <> show sym <> " at " <> show pos
codegenExp (A.OpExp left oper right _) = do
  leftOperand  <- codegenExp left
  rightOperand <- codegenExp right
  let f = case oper of
        A.PlusOp   -> L.add
        A.MinusOp  -> L.sub
        A.TimesOp  -> L.mul
        A.DivideOp -> L.sdiv
        _          -> error $ "unsupported operand " <> show oper
  f leftOperand rightOperand
codegenExp (A.IfExp test then' (Just else') _) = mdo
  -- %entry
  ---------
  testOp <- codegenExp test
  test'  <- L.icmp AST.NE testOp (L.int64 0)
  L.condBr test' ifThen ifElse

  -- %if.then
  -----------
  ifThen   <- L.block `L.named` "if.then"
  ifThenOp <- codegenExp then'
  L.br ifExit

  -- %if.else
  -----------
  ifElse   <- L.block `L.named` "if.else"
  ifElseOp <- codegenExp else'
  L.br ifExit

  -- %if.exit
  -----------
  ifExit <- L.block `L.named` "if.exit"
  L.phi [(ifThenOp, ifThen), (ifElseOp, ifElse)]
codegenExp (A.SeqExp expAndPosns) = do
  exps <- forM expAndPosns (\(e, _) -> codegenExp e)
  case exps of
    [] -> codegenExp A.NilExp
    _  -> pure $ last exps
codegenExp e = error $ "unimplemented alternative in codegenExp: " <> show e

emptyModule :: String -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = toShortBS label }

codegenTop :: A.Exp -> LLVM ()
codegenTop (A.LetExp decs body _) = do
  forM_ decs codegenDecl
  codegenFunDec $ A.FunDec
    { A.fundecName = S.Symbol "main"
    , A.params     = []
    , A.result     = Nothing
    , A.funBody    = body
    , A.funPos     = A.Pos { A.absChrOffset = -1, A.lineno = -1, A.colno = -1 }
    }

codegenTop e = error $ "implemented alternative in codegenTop: " <> show e

codegenDecl :: A.Dec -> LLVM ()
codegenDecl (A.FunctionDec [funDec]) = codegenFunDec funDec
codegenDecl _                        = undefined

codegenFunDec :: A.FunDec -> LLVM ()
codegenFunDec A.FunDec { A.fundecName = name, A.params = params, A.funBody = body }
  = do
    fun <- L.function (AST.Name $ toShortBS $ show name) args AST.i64 genBody
    registerOperand (S.name name) fun
 where
  args = toSig params

  toSig :: [A.Field] -> [(AST.Type, L.ParameterName)]
  toSig =
    fmap (\f -> (AST.i64, L.ParameterName $ toShortBS $ show $ A.fieldName f))

  genBody :: [AST.Operand] -> Codegen ()
  genBody ops = do
    _entry <- L.block `L.named` "entry"
    forM_ (zip ops params) $ \(op, field) -> do
      addr <- L.alloca AST.i64 Nothing 8
      L.store addr 8 op
      registerOperand (S.name $ A.fieldName field) addr
    codegenExp body >>= L.ret

codegenLLVM :: A.Exp -> AST.Module
codegenLLVM e =
  flip evalState (Env { operands = M.empty })
    $ L.buildModuleT "llvm-test"
    $ codegenTop e
