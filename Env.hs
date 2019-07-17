module Env where

import Symbol
import qualified Temp
import qualified Translate
import Types

import Data.Map (Map)
import qualified Data.Map as Map

data EnvEntry =
    VarEntry{ access :: Translate.X64Access
            , ty :: Ty }
  | FunEntry{ level :: Translate.X64Level
            , label :: Temp.Label
            , formals :: [Ty]
            , result :: Ty }
  deriving (Show)

type VEnv = Map Symbol Env.EnvEntry
type TEnv = Map Symbol Types.Ty

baseTEnv :: TEnv
baseTEnv = Map.fromList [(Symbol "string", STRING),
                         (Symbol "int", INT)]

outermost :: Translate.X64Level
outermost = Translate.X64Outermost

baseVEnv :: VEnv
baseVEnv = Map.fromList [
  let
    sym = Symbol "print"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[STRING]
                  , result=UNIT }),
  let
    sym = Symbol "flush"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[]
                  , result=UNIT }),
  let
    sym = Symbol "getchar"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[]
                  , result=STRING }),
  let
    sym = Symbol "ord"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[STRING]
                  , result=INT }),
  let
    sym = Symbol "chr"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[INT]
                  , result=STRING }),
  let
    sym = Symbol "size"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[STRING]
                  , result=INT }),
  let
    sym = Symbol "substring"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[STRING, INT, INT]
                  , result=STRING }),
  let
    sym = Symbol "concat"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[STRING, STRING]
                  , result=STRING }),
  let
    sym = Symbol "not"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[INT]
                  , result=INT }),
  let
    sym = Symbol "exit"
  in
    (sym, FunEntry{ level=outermost
                  , label=Temp.Label sym
                  , formals=[INT]
                  , result=UNIT })
  ]
