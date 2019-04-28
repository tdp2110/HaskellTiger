module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Translate as Translate
import qualified Types as Types

import qualified Data.Map
import Prelude hiding (exp)


newtype SemantError = SemantError String
instance Show SemantError where
  show (SemantError err) = "semantic issue: " ++ err

data ExpTy = ExpTy{exp :: Translate.Exp, ty :: Types.Ty }

transVar :: Env.VEnv -> Env.TEnv -> A.Var -> Either SemantError ExpTy
transExp :: Env.VEnv -> Env.TEnv -> A.Exp -> Either SemantError ExpTy
transDec :: Env.VEnv -> Env.TEnv -> A.Dec -> Either SemantError (Env.VEnv, Env.TEnv)
transTy :: Env.TEnv -> A.Ty -> Either SemantError Types.Ty

isArith :: A.Oper -> Bool
isArith A.PlusOp = True
isArith A.MinusOp = True
isArith A.TimesOp = True
isArith A.DivideOp = True
isArith _ = False

checkInt :: Types.Ty -> Maybe String-> Either String Translate.Exp
checkInt Types.INT _ = Right $ Translate.Exp ()
checkInt nonIntTy maybeCtx = Left $ (convertCtx maybeCtx) ++ "expected type Ty.INT, but found " ++ show nonIntTy
  where
    convertCtx Nothing = ""
    convertCtx (Just str) = str ++ ", "

transVar venv _ (A.SimpleVar sym pos) =
  case Data.Map.lookup sym venv of
    Just enventry -> case enventry of
                       Env.VarEntry{Env.ty=t} -> Right ExpTy{exp=Translate.Exp(), ty=t}
                       _ -> Left $ SemantError $ "variable " ++ (show sym) ++ " at " ++ (show pos) ++ " has no non-function bindings."
    Nothing -> Left $ SemantError $ "unbound free variable: " ++ (show sym) ++ " at " ++ (show pos)

transExp venv tenv (A.VarExp var) = transVar venv tenv var
transExp _ _ (A.IntExp _) = Right ExpTy{exp=Translate.Exp(), ty=Types.INT}
transExp _ _ (A.StringExp _) = Right ExpTy{exp=Translate.Exp(), ty=Types.STRING}
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
          Left err -> Left $ SemantError $ "In OpExp at " ++ show pos ++ ": " ++ err
          Right _ -> return ExpTy{exp=Translate.Exp(), ty=Types.INT}
      else undefined
transExp _ _ e = error $ "unimplemented transExp " ++ show e

transDec = undefined
transTy = undefined
