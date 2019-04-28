module Env where

import Types
import Symbol

import Data.Map (Map)
import qualified Data.Map as Map

data EnvEntry =
    VarEntry{ty :: Ty}
  | FunEntry{formals :: [Ty], result :: Ty}

type VEnv = Map Symbol Env.EnvEntry
type TEnv = Map Symbol Types.Ty

baseTEnv :: TEnv
baseTEnv = Map.fromList [(Symbol "string", STRING),
                         (Symbol "int", INT)]

baseVEnv :: VEnv
baseVEnv = Map.fromList [
  (Symbol "print", FunEntry{formals=[STRING], result=UNIT}),
  (Symbol "flush", FunEntry{formals=[], result=UNIT}),
  (Symbol "getchar", FunEntry{formals=[], result=STRING}),
  (Symbol "ord", FunEntry{formals=[STRING], result=INT}),
  (Symbol "chr", FunEntry{formals=[INT], result=STRING}),
  (Symbol "size", FunEntry{formals=[STRING], result=INT}),
  (Symbol "substring", FunEntry{formals=[STRING, INT, INT], result=STRING}),
  (Symbol "concat", FunEntry{formals=[STRING, STRING], result=STRING}),
  (Symbol "not", FunEntry{formals=[INT], result=INT}),
  (Symbol "exit", FunEntry{formals=[INT], result=UNIT})
  ]
