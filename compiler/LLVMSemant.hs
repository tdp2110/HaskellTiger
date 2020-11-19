{-# LANGUAGE OverloadedStrings #-}

module LLVMSemant where

import           LLVMTranslate                 as T

import qualified Absyn                         as A

import           LLVM.Module
import           LLVM.Context

import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.Float                as F
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP

import           Data.Word
import           Data.Int
import           Control.Monad.Except
import           Control.Applicative
import qualified Data.Map                      as Map


cgen :: A.Exp -> T.Codegen AST.Operand
cgen (A.IntExp i) =
  pure $ T.cons $ C.Int { C.integerBits = 64, C.integerValue = toInteger i }
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


cgenDecl :: A.Dec -> T.LLVM ()
cgenDecl (A.FunctionDec [funDec]) = cgenFunDec funDec
cgenDecl _                        = undefined

cgenFunDec :: A.FunDec -> T.LLVM ()
cgenFunDec A.FunDec { A.fundecName = _, A.params = _, A.result = _, A.funBody = _, A.funPos = _ }
  = undefined
