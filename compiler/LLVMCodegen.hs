{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module LLVMCodegen where

import qualified Absyn                         as A
import qualified Symbol                        as S

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Type                 as AST
import qualified LLVM.AST.Constant             as AST
import qualified LLVM.AST.IntegerPredicate     as AST

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L

import           Data.Word
import           Data.ByteString.Short
import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Control.Monad


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

data Env = Env { operands :: M.Map Text AST.Operand }
        deriving (Eq, Show)

registerOperand :: MonadState Env m => Text -> AST.Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

codegenExp :: A.Exp -> Codegen AST.Operand
codegenExp (A.IntExp i) = pure $ L.int64 $ toInteger i
codegenExp (A.VarExp (A.SimpleVar sym pos)) = do
  operandEnv <- gets operands
  case M.lookup (S.name sym) operandEnv of
    Just op -> L.load op 8
    Nothing -> error $ "use of undefined variable "  <> show sym <> " at " <> show pos
codegenExp (A.OpExp left oper right _) = do
  leftOperand <- codegenExp left
  rightOperand <- codegenExp right
  let f = case oper of
        A.PlusOp   -> L.add
        A.MinusOp  -> L.sub
        A.TimesOp  -> L.mul
        A.DivideOp -> L.sdiv
        _          -> error $ "unsupported operand " <> show oper
  f leftOperand rightOperand
codegenExp e = error $ "unimplemented alternative in codegenExp: " <> show e

emptyModule :: String -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = toShortBS label }

codegenTop :: A.Exp -> LLVM ()
codegenTop (A.LetExp decs _ _) = do
  _ <- forM decs codegenDecl
  pure ()

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
  let
      res =
          flip evalState (Env { operands = M.empty })
            $ L.buildModuleT "llvm-test"
            $ codegenTop e
  in  res
