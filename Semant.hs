module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Translate as Translate
import qualified Types as Types

import qualified Data.Map
import Data.Either
import Data.List
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

isCmp :: A.Oper -> Bool
isCmp A.EqOp = True
isCmp A.NeqOp = True
isCmp A.LtOp = True
isCmp A.GtOp = True
isCmp A.GeOp = True
isCmp _ = False

checkInt :: Types.Ty -> Maybe String-> Either String Translate.Exp
checkInt Types.INT _ = Right $ Translate.Exp ()
checkInt nonIntTy maybeCtx = Left $ (convertCtx maybeCtx) ++
                             "expected type Ty.INT, but found " ++ show nonIntTy
  where
    convertCtx Nothing = ""
    convertCtx (Just str) = str ++ ", "

emptyExp :: Translate.Exp
emptyExp = Translate.Exp()

transVar venv _ (A.SimpleVar sym pos) =
  case Data.Map.lookup sym venv of
    Just Env.VarEntry{Env.ty=t} -> Right ExpTy{exp=emptyExp, ty=t}
    Just (Env.FunEntry _ _) -> Left SemantError{
      what="variable " ++ (show sym) ++ " has no non-function bindings.",
      at=pos}
    Nothing -> Left SemantError{
      what="unbound free variable: " ++ (show sym),
      at=pos}
transVar venv tenv (A.FieldVar var sym pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
  case varTy of
    r@(Types.RECORD(sym2ty, _)) -> case lookup sym sym2ty of
                                 Just t -> return ExpTy{exp=emptyExp, ty=t}
                                 Nothing -> Left SemantError{
                                   what="in field expr, record type " ++
                                        (show r) ++ " has no " ++ (show sym) ++ " field",
                                   at=pos}
    t@(_) -> Left SemantError{
      what="in field expr, only record types have fields. type=" ++ (show t),
      at=pos}
transVar venv tenv (A.SubscriptVar var expr pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
  case varTy of
    Types.ARRAY(varEltTy, _) -> do
      ExpTy{exp=_, ty=expTy} <- transExp venv tenv expr
      case expTy of
        Types.INT -> return ExpTy{exp=emptyExp, ty=varEltTy}
        nonIntTy@(_) -> Left SemantError{
          what="in subscript expr, subscript type is not an INT, is an " ++ (show nonIntTy),
          at=pos}
    nonArrayTy@(_) -> Left SemantError{
      what="in subscript expr, only arrays may be subscripted -- attempting to subscript type=" ++
           (show nonArrayTy),
      at=pos}

transExp venv tenv (A.VarExp var) = transVar venv tenv var
transExp _ _ A.NilExp = Right ExpTy{exp=emptyExp, ty=Types.NIL}
transExp _ _ (A.IntExp _) = Right ExpTy{exp=emptyExp, ty=Types.INT}
transExp _ _ (A.StringExp _) = Right ExpTy{exp=emptyExp, ty=Types.STRING}
transExp venv tenv (A.CallExp funcSym argExps pos) =
  case Data.Map.lookup funcSym venv of
    Just (Env.FunEntry formalsTys resultTy) ->
      case sequence $ map (transExp venv tenv) argExps of
        Left err -> Left err
        Right paramExpTys ->
          let paramTys = map ty paramExpTys in
            if (length formalsTys) /= (length paramTys) then
              Left SemantError{what="function " ++ (show funcSym) ++
                                        " expects " ++ (show $ length formalsTys) ++
                                        " parameters but was passed " ++ (show $ length paramTys),
                                   at=pos}
              else case filter (\(ty1, ty2, _) -> ty1 /= ty2)
                        (zip3 formalsTys paramTys [0 :: Integer ..]) of
                     [] -> Right ExpTy{exp=emptyExp, ty=resultTy}
                     ((formalTy, paramTy, ix):_) -> Left SemantError{
                       what="parameter " ++ (show ix) ++ " of func " ++ (show funcSym) ++
                            " requires type " ++ (show formalTy) ++ " but was passed a value of type " ++
                            (show paramTy),
                       at=pos}
    Just (Env.VarEntry t) -> Left SemantError{
      what="only functions are callable -- found type " ++ (show t),
      at=pos}
    Nothing -> Left SemantError{
      what="unbound free variable " ++ (show funcSym),
      at=pos}
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
          Left err -> Left SemantError{
            what="In OpExp, " ++ err,
            at=pos}
          Right _ -> return ExpTy{exp=emptyExp, ty=Types.INT}
      else if isCmp op then
             let cmpReturn = return ExpTy{exp=emptyExp, ty=Types.INT} in
               case (tyleft, tyright) of
                 (Types.INT, Types.INT) -> cmpReturn
                 (Types.STRING, Types.STRING) -> cmpReturn
                 (r1@(Types.RECORD _), r2@(Types.RECORD _)) ->
                   if r1 == r2 then cmpReturn
                   else Left SemantError{
                     what="only identical record types may be compared",
                     at=pos}
                 (arr1@(Types.ARRAY _), arr2@(Types.ARRAY _)) ->
                   if arr1 == arr2 then cmpReturn
                   else Left SemantError{
                     what="only identical array types may be compared",
                     at=pos}
                 _ -> Left SemantError{
                   what="incomparable types " ++ (show tyleft) ++ " and " ++ (show tyright),
at=pos}
             else undefined

transExp venv tenv A.RecordExp{A.fields=fieldSymExpPosns,
                               A.typ=typSym,
                               A.pos=pos} =
  case Data.Map.lookup typSym tenv of
    Nothing -> Left SemantError{
      what="unbound free type variable " ++ (show typSym),
      at=pos}
    Just maybeRecordTy ->
      case maybeRecordTy of
        recordTy@(Types.RECORD(sym2ty, _)) ->
          let
            expectedSyms = map fst sym2ty
            actualSyms = map (\(sym,_,_) -> sym) fieldSymExpPosns
          in
            if actualSyms /= expectedSyms then
              Left SemantError{what="incompatible field names: expected " ++
                                (show expectedSyms) ++ " but record expression has " ++
                                (show actualSyms),
                               at=pos}
            else
              case sequence $ map (\(_,expr,_) -> transExp venv tenv expr) fieldSymExpPosns of
                Left err -> Left err
                Right actualFieldExpTys ->
                  let
                    expectedFieldTys = map snd sym2ty
                    actualFieldTys = map ty actualFieldExpTys
                    fieldPosns = map (\(_,_,fieldPos) -> fieldPos) fieldSymExpPosns
                  in
                    case filter (\(_,expectedTy,actualTy,_) -> expectedTy == actualTy)
                         (zip4 expectedSyms expectedFieldTys actualFieldTys fieldPosns) of
                      [] -> Right ExpTy{exp=emptyExp, ty=recordTy}
                      ((sym,expectedTy,actualTy,fieldPos):_) ->
                        Left SemantError{what="in record exp, field " ++ (show sym) ++
                                              " should have type " ++ (show expectedTy) ++
                                              " but has type " ++ (show actualTy),
                                         at=fieldPos}
        _ -> Left SemantError{
          what="only record types may appear as the symbol in a record instance declaration",
          at=pos}
transExp venv tenv (A.SeqExp(expAndPosns)) =
  case sequence $ map (\(expr,_) -> transExp venv tenv expr) expAndPosns of
    Left err -> Left err
    Right [] -> Right ExpTy{exp=emptyExp, ty=Types.UNIT}
    Right expTys -> Right $ last expTys
transExp venv tenv A.AssignExp{A.var=var, A.exp=expr, A.pos=pos} =
  do
    ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
    ExpTy{exp=_, ty=exprTy} <- transExp venv tenv expr
    if varTy == exprTy then
      return ExpTy{exp=emptyExp, ty=Types.UNIT}
      else Left SemantError{what="in assignExp, variable has type " ++ (show varTy) ++
                                 " but assign target has type " ++ (show exprTy),
                            at=pos}
transExp venv tenv A.IfExp{A.test=testExpr,
                           A.then'=thenExpr,
                           A.else'=maybeElseExpr,
                           A.pos=pos} =
  let transexp = transExp venv tenv in
    do
      testExpTy <- transexp testExpr
      thenExpTy <- transexp thenExpr
      let maybeElseExpTy = fmap transexp maybeElseExpr in
        if (ty testExpTy) /= Types.INT then
          Left SemantError{what="in ifExp, test expressions must be integral: " ++
                           "found type=" ++ (show $ ty testExpTy),
                           at=pos}
          else case maybeElseExpTy of
                 Nothing -> return thenExpTy
                 Just elseExpTyEither -> do
                   elseExpTy <- elseExpTyEither
                   let
                     thenTy = ty thenExpTy
                     elseTy = ty elseExpTy
                     in
                     if thenTy /= elseTy then
                       Left SemantError{what="in ifExp, thenExp and elseExp must have " ++
                                             "the same type: found " ++ (show thenTy) ++
                                             " and " ++ (show elseTy) ++
                                             ", respectfully",
                                        at=pos}
                     else return ExpTy{exp=emptyExp, ty=thenTy}
transExp _ _ e = error $ "unimplemented transExp " ++ show e

transDec = undefined
transTy = undefined
