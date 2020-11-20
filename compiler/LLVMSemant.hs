{-# LANGUAGE OverloadedStrings #-}

module LLVMSemant where

import           LLVMTranslate                 as T

import qualified Absyn                         as A

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Type                 as T
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.IntegerPredicate     as IP

import           Control.Monad

toConstI64 :: Int -> C.Constant
toConstI64 i = C.Int { C.integerBits = 64, C.integerValue = toInteger i }

codegenTop :: A.Exp -> T.LLVM ()
codegenTop (A.LetExp decs _ _) = do
  _ <- forM decs cgenDecl
  define T.i64 "main" [] blks
 where
  blks = createBlocks $ execCodegen $ do
    entryBlk <- addBlock entryBlockName
    _        <- setBlock entryBlk
    --cgen body >>= ret
    cgen (A.IntExp 42) >>= ret
codegenTop e = error $ "implemented alternative in codegenTop: " <> show e

cgen :: A.Exp -> T.Codegen AST.Operand
cgen (A.IntExp i             ) = pure $ T.cons $ toConstI64 i
cgen (A.OpExp left op right _) = do
  leftOperand  <- cgen left
  rightOperand <- cgen right
  let f = case op of
        A.PlusOp   -> T.add
        A.MinusOp  -> T.sub
        A.TimesOp  -> T.mul
        A.DivideOp -> T.div
        _          -> error $ "unsupported operand " <> show op
  f leftOperand rightOperand
cgen (A.IfExp test then' (Just else') _) = do
  ifThen  <- addBlock "if.then"
  ifElse  <- addBlock "if.else"
  ifExit  <- addBlock "if.exit"

  -- %entry
  ----------
  cond    <- cgen test
  test'   <- T.icmp IP.NE cond $ T.cons $ toConstI64 0
  _       <- cbr test' ifThen ifElse

  -- if.then
  ------------------
  _       <- setBlock ifThen
  trval   <- cgen then'
  _       <- br ifExit
  ifThen' <- getBlock

  -- if.else
  ------------------
  _       <- setBlock ifElse
  flval   <- cgen else'
  _       <- br ifExit
  ifElse' <- getBlock

  -- if.exit
  ------------------
  _       <- setBlock ifExit
  T.phi T.i64 [(trval, ifThen'), (flval, ifElse')]
cgen e = error $ "unimplemented cgen alternative: " <> show e

cgenDecl :: A.Dec -> T.LLVM ()
cgenDecl (A.FunctionDec [funDec]) = cgenFunDec funDec
cgenDecl _                        = undefined

cgenFunDec :: A.FunDec -> T.LLVM ()
cgenFunDec A.FunDec { A.fundecName = name, A.params = params, A.funBody = body }
  = define T.i64 (show name) fnargs blks
 where
  fnargs = toSig params
  blks   = createBlocks $ execCodegen $ do
    entry' <- addBlock entryBlockName
    _      <- setBlock entry'
    _      <- forM params $ \p -> do
      let pname = show $ A.fieldName p
      var <- alloca T.i64
      _   <- store var $ local $ AST.Name $ toShortBS $ pname
      assign pname var
    cgen body >>= ret


toSig :: [A.Field] -> [(AST.Type, AST.Name)]
toSig = fmap (\f -> (T.i64, AST.Name $ toShortBS $ show $ A.fieldName f))
