module Semant where

import qualified Absyn as A
import qualified Env as Env
import Symbol
import qualified Translate as Translate
import qualified Types as Types

import Prelude hiding (exp)
import Data.Map (Map)
import qualified Data.Map as Map

newtype SemaError = SemaError String
instance Show SemaError where
  show (SemaError err) = "semantic issue: " ++ err

type VEnv = Map Symbol Env.EnvEntry
type TEnv = Map Symbol Types.Ty
data ExpTy = ExpTy{exp :: Translate.Exp, ty :: Types.Ty }

transVar :: VEnv -> VEnv -> A.Var -> ExpTy
transExp :: VEnv -> TEnv -> A.Exp -> Either SemaError ExpTy
transDec :: VEnv -> TEnv -> A.Dec -> (VEnv, TEnv)
transTy :: TEnv -> A.Ty -> Types.Ty

isArith :: A.Oper -> Bool
isArith A.PlusOp = True
isArith A.MinusOp = True
isArith A.TimesOp = True
isArith A.DivideOp = True
isArith _ = False

checkInt :: Types.Ty -> Maybe String-> Either String Translate.Exp
checkInt Types.INT _ = Right $ Translate.Exp ()
checkInt nonIntTy maybeCtx = Left $ (convertCtx maybeCtx) ++ "expected Ty.INT. Computed Type=" ++ show nonIntTy
  where
    convertCtx Nothing = ""
    convertCtx (Just str) = str ++ ", "

transVar = undefined
transExp _ _ (A.IntExp i) = Right ExpTy{exp=Translate.Exp(), ty=Types.INT}
transExp venv tenv A.OpExp{A.left=leftExp,
                           A.oper=op,
                           A.right=rightExp,
                           A.pos=pos} =
  do
    ExpTy{exp=_, ty=tyleft} <- transExp venv tenv leftExp
    ExpTy{exp=_, ty=tyright} <- transExp venv tenv rightExp
    if isArith op then
      let maybeError = do
                  checkInt tyleft (Just "in left hand operand")
                  checkInt tyright (Just "in right hand operand") in
        case maybeError of
          Left err -> Left $ SemaError $ "In OpExp at " ++ show pos ++ ": " ++ err
          Right _ -> return ExpTy{exp=Translate.Exp(), ty=Types.INT}
      else undefined

transDec = undefined
transTy = undefined
