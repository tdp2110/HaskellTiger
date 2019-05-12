module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Translate as Translate
import qualified Types as Types
import Symbol

import qualified Data.Map as Map
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
transLetDecs :: Env.VEnv -> Env.TEnv -> [A.Dec] -> A.Pos -> Either SemantError (Env.VEnv, Env.TEnv)

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
  case Map.lookup sym venv of
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

data BreakContext = CanBreak | CannotBreak

transExp venv tenv expr =
  transExp' venv tenv CannotBreak expr

transExp' :: Env.VEnv -> Env.TEnv -> BreakContext -> A.Exp -> Either SemantError ExpTy

transExp' venv tenv _ (A.VarExp var) = transVar venv tenv var
transExp' _ _ _ A.NilExp = Right ExpTy{exp=emptyExp, ty=Types.NIL}
transExp' _ _ _ (A.IntExp _) = Right ExpTy{exp=emptyExp, ty=Types.INT}
transExp' _ _ _ (A.StringExp _) = Right ExpTy{exp=emptyExp, ty=Types.STRING}
transExp' venv tenv breakContext (A.CallExp funcSym argExps pos) =
  case Map.lookup funcSym venv of
    Just (Env.FunEntry formalsTys resultTy) ->
      case sequence $ map (transExp' venv tenv breakContext) argExps of
        Left err -> Left err
        Right paramExpTys ->
          let paramTys = map ty paramExpTys in
            if (length formalsTys) /= (length paramTys) then
              Left SemantError{what="function " ++ (show funcSym) ++
                                        " expects " ++ (show $ length formalsTys) ++
                                        " parameters but was passed " ++ (show $ length paramTys),
                                   at=pos}
              else
              case filter (\(ty1, ty2, _) -> ty1 /= ty2)
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
transExp' venv tenv breakContext (A.OpExp leftExp op rightExp pos) =
  do
    ExpTy{exp=_, ty=tyleft} <- transExp' venv tenv breakContext leftExp
    ExpTy{exp=_, ty=tyright} <- transExp' venv tenv breakContext rightExp
    if isArith op then
      let maybeError = do
            checkInt tyleft (Just "in left hand operand")
            checkInt tyright (Just "in right hand operand") in
        case maybeError of
          Left err -> Left SemantError{
            what="In OpExp, " ++ err,
            at=pos}
          Right _ -> return ExpTy{exp=emptyExp, ty=Types.INT}
      else
      if isCmp op then
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

transExp' venv tenv breakContext (A.RecordExp fieldSymExpPosns typSym pos) =
  case Map.lookup typSym tenv of
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
              case sequence $ map (\(_,expr,_) -> transExp' venv tenv breakContext expr) fieldSymExpPosns of
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
        t@(_) -> Left SemantError{
          what="only record types may appear as the symbol in a record instance " ++
               "definition. Found type=" ++ (show t),
          at=pos}
transExp' venv tenv breakContext (A.SeqExp expAndPosns) =
  case sequence $ map (\(expr,_) -> transExp' venv tenv breakContext expr) expAndPosns of
    Left err -> Left err
    Right [] -> Right ExpTy{exp=emptyExp, ty=Types.UNIT}
    Right expTys -> Right $ last expTys
transExp' venv tenv breakContext (A.AssignExp var expr pos) =
  do
    ExpTy{exp=_, ty=varTy} <- transVar venv tenv var
    ExpTy{exp=_, ty=exprTy} <- transExp' venv tenv breakContext expr
    if varTy == exprTy then
      return ExpTy{exp=emptyExp, ty=Types.UNIT}
      else
      Left SemantError{what="in assignExp, variable has type " ++ (show varTy) ++
                            " but assign target has type " ++ (show exprTy),
                       at=pos}
transExp' venv tenv breakContext
  (A.IfExp testExpr thenExpr maybeElseExpr pos) =
  let transexp = transExp' venv tenv breakContext in
    do
      testExpTy <- transexp testExpr
      thenExpTy <- transexp thenExpr
      let maybeElseExpTy = fmap transexp maybeElseExpr in
        if (ty testExpTy) /= Types.INT then
          Left SemantError{what="in ifExp, the test expression must be integral: " ++
                           "found type=" ++ (show $ ty testExpTy),
                           at=pos}
        else
          case maybeElseExpTy of
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
                else
                  return ExpTy{exp=emptyExp, ty=thenTy}
transExp' venv tenv breakContext (A.WhileExp testExp bodyExp pos) =
  do
    ExpTy{exp=_, ty=testTy} <- transExp' venv tenv breakContext testExp
    _ <- transExp' venv tenv CanBreak bodyExp
    if testTy /= Types.INT then
      Left SemantError{what="in whileExp, the test expression must be integral: " ++
                            "found type=" ++ (show testTy),
                       at=pos}
      else
      return ExpTy{exp=emptyExp, ty=Types.UNIT}
transExp' _ _ breakContext (A.BreakExp pos) =
  case breakContext of
    CanBreak -> Right ExpTy{exp=emptyExp, ty=Types.UNIT}
    CannotBreak -> Left SemantError{what="break expression not enclosed in a while or for",
                                    at=pos}
transExp' venv tenv breakContext (A.ArrayExp arrayTySym sizeExp initExp pos) =
  case Map.lookup arrayTySym tenv of
    Nothing -> Left SemantError{
      what="unbound free type variable " ++ (show arrayTySym),
      at=pos}
    Just maybeArrayTy ->
      case maybeArrayTy of
        arrayTy@(Types.ARRAY(arrayEltTy,_)) -> do
          ExpTy{exp=_, ty=sizeTy} <- transExp' venv tenv breakContext sizeExp
          ExpTy{exp=_, ty=initTy} <- transExp' venv tenv breakContext initExp
          if sizeTy /= Types.INT then
            Left SemantError{what="in ArrayExp, sizeExp must be an integer. " ++
                                  "Found type=" ++ (show sizeTy),
                             at=pos}
            else
            if initTy /= arrayEltTy then
              Left SemantError{what="in ArrayExp, initExp has actual type " ++
                                    (show initTy) ++ ", when it must have " ++
                                    (show arrayEltTy),
                               at=pos}
            else
              return ExpTy{exp=emptyExp, ty=arrayTy}
        t@(_) -> Left SemantError{
          what="only array types may appear as the symbol in an array instance " ++
               "definition. Found type=" ++ (show t),
          at=pos}
transExp' venv tenv breakContext (A.ForExp forVar _ loExp hiExp body pos) =
  let bodyVEnv = Map.insert forVar Env.VarEntry{Env.ty=Types.INT} venv in
    do
      ExpTy{exp=_, ty=loTy} <- transExp' venv tenv breakContext loExp
      ExpTy{exp=_, ty=hiTy} <- transExp' venv tenv breakContext hiExp
      ExpTy{exp=_, ty=bodyTy} <- transExp' bodyVEnv tenv CanBreak body
      if (loTy /= Types.INT) || (hiTy /= Types.INT) then
        Left SemantError{what="only integer expressions may appear as bounds in a ForExp",
                       at=pos}
        else
        if bodyTy /= Types.UNIT then
          Left SemantError{what="the body of a ForExp must yield no value",
                           at=pos}
        else
          case checkForVarNotAssigned forVar body of
            Left err -> Left err
            _ -> return ExpTy{exp=emptyExp, ty=Types.UNIT}
transExp' venv tenv breakContext (A.LetExp decs bodyExp letPos) = do
  (venv', tenv') <- transLetDecs venv tenv decs letPos
  transExp' venv' tenv' breakContext bodyExp

transLetDecs venv tenv decls letPos =
  case checkDeclNamesDistinctInLet decls letPos of
    Left err -> Left err
    _ -> case sequence $ map (transDec venv tenv) decls of
      Left err -> Left err
      Right processedDecls -> Right $ mergeEnvs processedDecls

mergeEnvs :: [(Env.VEnv, Env.TEnv)] -> (Env.VEnv, Env.TEnv)
mergeEnvs listOfEnvs =
  foldl' step (Map.empty, Map.empty) listOfEnvs
  where
    step (venv, tenv) (venv', tenv') = (Map.union venv venv', Map.union tenv tenv')

checkDeclNamesDistinctInLet :: [A.Dec] -> A.Pos -> Either SemantError ()
checkDeclNamesDistinctInLet decls letPos =
  let flattenedDecls = flattenDecls decls in
    case foldl' step (Right (Map.empty, Map.empty)) flattenedDecls of
      Left err -> Left err
      Right _ -> Right ()
      where
        step (Left err) _ = Left err
        step (Right (funAndVarSyms, tySyms)) (TyDec name decPos) =
          case Map.lookup name tySyms of
            Nothing -> Right (funAndVarSyms, Map.insert name decPos tySyms)
            Just decPos' -> Left SemantError{
              what="multiple type declarations with name " ++ (show name) ++
                   " in letExp declarations at " ++ (show decPos') ++
                   " and " ++ (show decPos),
              at=letPos}
        step (Right (funAndVarSyms, tySyms)) decl@_ =
          let
            name = declName decl
            decPos = declPos decl
          in
            case Map.lookup name funAndVarSyms of
              Nothing -> Right (Map.insert name decPos funAndVarSyms, tySyms)
              Just decPos' -> Left SemantError{
                what="multiple function or value declarations of symbol " ++
                     (show name) ++ " in letExp declarations at " ++ (show decPos') ++
                     " and " ++ (show decPos),
                at=letPos}

data DeclElt =
  VarDec{declName :: Symbol, declPos :: A.Pos}
  | FunDec{declName :: Symbol, declPos :: A.Pos}
  | TyDec{declName :: Symbol, declPos :: A.Pos}

flattenDecls :: [A.Dec] -> [DeclElt]
flattenDecls decls = do
  decl <- decls
  case decl of
    A.FunctionDec funDecs -> map (\funDec -> FunDec (A.fundecName funDec) (A.funPos funDec)) funDecs
    A.VarDec name _ _ _ posn -> return $ VarDec name posn
    A.TypeDec tydecs -> map (\tyDec -> TyDec (A.tydecName tyDec) (A.tydecPos tyDec)) tydecs

checkForVarNotAssigned :: Symbol -> A.Exp -> Either SemantError ()
checkForVarNotAssigned forVar (A.CallExp _ exps _) =
  case sequence $ map (\e -> checkForVarNotAssigned forVar e) exps of
    Left err -> Left err
    Right _ -> Right ()
checkForVarNotAssigned forVar (A.OpExp leftExp _ rightExp _) = do
  checkForVarNotAssigned forVar leftExp
  checkForVarNotAssigned forVar rightExp
checkForVarNotAssigned forVar (A.RecordExp fields _ _) =
  case sequence $ map (\(_,e,_) -> checkForVarNotAssigned forVar e) fields of
    Left err -> Left err
    Right _ -> Right ()
checkForVarNotAssigned forVar (A.SeqExp seqElts) =
  case sequence $ map (\(e,_) -> checkForVarNotAssigned forVar e) seqElts of
    Left err -> Left err
    Right _ -> Right ()
checkForVarNotAssigned forVar (A.AssignExp (A.SimpleVar var _) e pos) =
  if forVar == var then
    Left SemantError{what="forVar assigned in forBody",
                     at=pos}
  else
    checkForVarNotAssigned forVar e
checkForVarNotAssigned forVar (A.IfExp testExp thenExp maybeElseExp _) = do
  checkForVarNotAssigned forVar testExp
  checkForVarNotAssigned forVar thenExp
  case fmap (\e -> checkForVarNotAssigned forVar e) maybeElseExp of
    Just (Left err) -> Left err
    _ -> Right ()
checkForVarNotAssigned forVar (A.WhileExp testExp bodyExp _) = do
  checkForVarNotAssigned forVar testExp
  checkForVarNotAssigned forVar bodyExp
checkForVarNotAssigned forVar (A.ForExp _ _ loExp hiExp bodyExp _) = do
  checkForVarNotAssigned forVar loExp
  checkForVarNotAssigned forVar hiExp
  checkForVarNotAssigned forVar bodyExp
checkForVarNotAssigned forVar (A.ArrayExp _ sizeExp initExp _) = do
  checkForVarNotAssigned forVar sizeExp
  checkForVarNotAssigned forVar initExp
checkForVarNotAssigned forVar (A.LetExp decs bodyExp _) =
  let
    forVarIsRebound = all forVarIsReboundAtDec decs
    forVarIsReboundAtDec (A.VarDec varName _ _ _ _) = varName == forVar
    forVarIsReboundAtDec _ = False
      in
    if forVarIsRebound then
      Right ()
    else
      checkForVarNotAssigned forVar bodyExp
checkForVarNotAssigned _ _ = Right ()

transDec = undefined
transTy = undefined
