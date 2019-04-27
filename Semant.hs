module Semant where

import qualified Absyn as A
import Env
import Symbol
import Translate
import Types

import Data.Map (Map)
import qualified Data.Map as Map

type VEnv = Map Symbol Env.EnvEntry
type TEnv = Map Symbol Types.Ty
data ExpTy = ExpTy{exp :: Translate.Exp, ty :: Types.Ty }

transVar :: VEnv -> VEnv -> A.Var -> ExpTy
transExp :: VEnv -> TEnv -> A.Exp -> ExpTy
transDec :: VEnv -> TEnv -> A.Dec -> (VEnv, TEnv)
transTy :: TEnv -> A.Ty -> Types.Ty

transVar = undefined
transExp venv tenv A.OpExp{A.left=func, A.oper=A.PlusOp, A.right=rightExp, A.pos=pos} = undefined
transDec = undefined
transTy = undefined
