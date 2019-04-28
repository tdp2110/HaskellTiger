module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Translate as Translate
import qualified Types as Types

import qualified Data.Map
import Prelude hiding (exp)


data SemantError = SemantError{what :: String, at :: A.Pos} deriving (Eq)
instance Show SemantError where
  show (SemantError err pos) = "semantic issue at " ++ (show pos) ++ ": " ++ (show err)

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
checkInt nonIntTy maybeCtx = Left $ (convertCtx maybeCtx) ++
                             "expected type Ty.INT, but found " ++ show nonIntTy
  where
    convertCtx Nothing = ""
    convertCtx (Just str) = str ++ ", "

transVar venv _ (A.SimpleVar sym pos) =
  case Data.Map.lookup sym venv of
    Just Env.VarEntry{Env.ty=t} -> Right ExpTy{exp=Translate.Exp(), ty=t}
    Just (Env.FunEntry _ _) -> Left $ SemantError{
      what="variable " ++ (show sym) ++ " has no non-function bindings.",
      at=pos}
    Nothing -> Left $ SemantError{
      what="unbound free variable: " ++ (show sym),
      at=pos}
transVar venv tenv (A.FieldVar var sym pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
  case varTy of
    r@(Types.RECORD(sym2ty, _)) -> case lookup sym sym2ty of
                                 Just t -> return ExpTy{exp=Translate.Exp(), ty=t}
                                 Nothing -> Left $ SemantError{
                                   what="in field expr, record type " ++
                                        (show r) ++ " has no " ++ (show sym) ++ " field",
                                   at=pos}
    t@(_) -> Left $ SemantError{
      what="in field expr, only record types have fields. type=" ++ (show t),
      at=pos}
transVar venv tenv (A.SubscriptVar var expr pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
  case varTy of
    Types.ARRAY(varEltTy, _) -> do
      ExpTy{exp=_, ty=expTy} <- transExp venv tenv expr
      case expTy of
        Types.INT -> return ExpTy{exp=Translate.Exp(), ty=varEltTy}
        nonIntTy@(_) -> Left $ SemantError{
          what="in subscript expr, subscript type is not an INT, is an " ++ (show nonIntTy),
          at=pos}
    nonArrayTy@(_) -> Left $ SemantError{
      what="in subscript expr, only arrays may be subscripted -- attempting to subscript type=" ++
           (show nonArrayTy),
      at=pos}

transExp venv tenv (A.VarExp var) = transVar venv tenv var
transExp _ _ A.NilExp = Right ExpTy{exp=Translate.Exp(), ty=Types.NIL}
transExp _ _ (A.IntExp _) = Right ExpTy{exp=Translate.Exp(), ty=Types.INT}
transExp _ _ (A.StringExp _) = Right ExpTy{exp=Translate.Exp(), ty=Types.STRING}
transExp venv tenv (A.CallExp funcSym argExps pos) =
  case Data.Map.lookup funcSym venv of
    Just (Env.VarEntry t) -> undefined
    Just (Env.FunEntry paramTys resultTy) -> undefined
    Nothing -> undefined
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
          Left err -> Left $ SemantError{
            what="In OpExp, " ++ err,
            at=pos}
          Right _ -> return ExpTy{exp=Translate.Exp(), ty=Types.INT}
      else undefined
transExp _ _ e = error $ "unimplemented transExp " ++ show e

transDec = undefined
transTy = undefined
