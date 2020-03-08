module Env where

import qualified Frame
import qualified X64Frame
import Symbol
import qualified Temp
import qualified Translate
import Types

import Data.List
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

baseVEnv :: X64Frame.X64 -> Temp.Generator -> (VEnv, Temp.Generator)
baseVEnv x64 gen =
  let
    signatures = [ ("print", [STRING], UNIT)
                 , ("flush", [], UNIT)
                 , ("getchar", [], STRING)
                 , ("getline", [], STRING)
                 , ("ord", [STRING], INT)
                 , ("chr", [INT], STRING)
                 , ("itoa", [INT], STRING)
                 , ("size", [STRING], INT)
                 , ("substring", [STRING, INT, INT], STRING)
                 , ("concat", [STRING, STRING], STRING)
                 , ("not", [INT], INT)
                 , ("exit", [INT], UNIT)
                 ]
    (builtinList, gen') = foldl'
                          enterBuiltin
                          ([], gen)
                          signatures
    builtinMap = Map.fromList builtinList
  in
    (builtinMap, gen')
  where
    enterBuiltin :: ([(Symbol, EnvEntry)], Temp.Generator)
                 -> (String, [Types.Ty], Types.Ty)
                 -> ([(Symbol, EnvEntry)], Temp.Generator)
    enterBuiltin (acc, gen') (nm, formalTys, returnTy) =
      let
        sym = Symbol nm
        lab = Temp.Label $ Symbol $ "tiger_" ++ nm
        escapes = fmap (\_ -> Frame.DoesNotEscape) formalTys
        (gen'', lev) = Translate.x64NewLevel
                         x64
                         Nothing
                         (Translate.X64Outermost, lab, escapes)
                         gen'
        entry = (sym, FunEntry { level=lev
                               , label=lab
                               , formals=formalTys
                               , result=returnTy })
      in
        (acc ++ [entry], gen'')
