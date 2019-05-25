module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Translate as Translate
import qualified Types as Types
import Symbol

import Control.Monad (join)
import qualified Data.Map as Map
import Data.Either
import Data.List
import Data.Graph
import Prelude hiding (exp)

data SemantError = SemantError{what :: String, at :: A.Pos} deriving (Eq)
instance Show SemantError where
  show (SemantError err pos) = "semantic issue at " ++ (show pos) ++ ": " ++ (show err)

data ExpTy = ExpTy{exp :: Translate.Exp, ty :: Types.Ty } deriving (Show)

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

actualTy :: Types.Ty -> Types.Ty
actualTy typ = typ


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
            if (length formalsTys) /= (length paramTys)
            then
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
      what="only functions are callable: found type " ++ (show t),
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
      if isCmp op
      then
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
            if actualSyms /= expectedSyms
            then
              Left SemantError{what="incompatible field names: expected " ++
                                (show expectedSyms) ++ " but record expression has " ++
                                (show actualSyms),
                               at=pos}
            else
              case sequence $ map (\(_,expr,_) -> transExp' venv tenv breakContext expr) fieldSymExpPosns of
                Left err -> Left err
                Right actualFieldExpTys ->
                  let
                    expectedFieldTys = map (actualTy . snd) sym2ty
                    actualFieldTys = map ty actualFieldExpTys
                    fieldPosns = map (\(_,_,fieldPos) -> fieldPos) fieldSymExpPosns
                  in
                    case filter (\(_,expectedTy,actualTy',_) -> expectedTy /= actualTy')
                         (zip4 expectedSyms expectedFieldTys actualFieldTys fieldPosns) of
                      [] -> Right ExpTy{exp=emptyExp, ty=recordTy}
                      ((sym,expectedTy,actualTy',fieldPos):_) ->
                        Left SemantError{what="in record exp, field " ++ (show sym) ++
                                              " should have type " ++ (show expectedTy) ++
                                              " but has type " ++ (show actualTy'),
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
    if varTy == exprTy
    then
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
        if (ty testExpTy) /= Types.INT
        then
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
                if thenTy /= elseTy
                then
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
    if testTy /= Types.INT
    then
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
          if sizeTy /= Types.INT
          then
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
      if (loTy /= Types.INT) || (hiTy /= Types.INT)
      then
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
    _ -> foldl' step (Right (venv, tenv)) decls
      where
        step (Left err) _ = Left err
        step (Right (venv', tenv')) decl =
          transDec venv' tenv' decl

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
  if forVar == var
  then
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
    if forVarIsRebound
    then
      Right ()
    else
      checkForVarNotAssigned forVar bodyExp
checkForVarNotAssigned _ _ = Right ()

transDec venv tenv (A.VarDec name _ maybeTypenameAndPos initExp posn) =
  let maybeTypeAnnotation =
        join $ fmap (\(typename,_) -> Map.lookup typename tenv) maybeTypenameAndPos in
    case transExp venv tenv initExp of
      Left err -> Left err
      Right ExpTy{exp=_, ty=actualInitTy} ->
        if actualInitTy == Types.NIL
        then
          case maybeTypeAnnotation of
            Just recTy@(Types.RECORD _) ->
              Right (Map.insert name (Env.VarEntry recTy) venv, tenv)
            _ -> Left SemantError{
              what="nil expression declarations must be constrained by a RECORD type",
              at=posn}
        else
          let result = Right (Map.insert name (Env.VarEntry actualInitTy) venv, tenv)
          in
            case maybeTypeAnnotation of
              Just typeAnnotation ->
                if typeAnnotation /= actualInitTy
                then
                  Left SemantError{what="mismatch in type annotation and computed type in varDecl: " ++
                                        "type annotation " ++ (show typeAnnotation) ++
                                        ", computed type " ++ (show actualInitTy),
                                   at=posn}
                else result
              Nothing -> result
transDec venv tenv (A.FunctionDec []) = Right (venv, tenv)
transDec venv tenv (A.FunctionDec ((A.FunDec funName params result bodyExp pos):[])) =
  let
    maybeResultTy =
      join $ fmap (\(typename,_) -> Map.lookup typename tenv) result
    maybeFormalsTys =
      sequence $ fmap computeFormalTy params
  in
    case maybeFormalsTys of
      Left err -> Left err
      Right formalsTys ->
        let
          resultTy = case maybeResultTy of
                       Nothing -> Types.UNIT
                       Just typ -> typ
          venv' = Map.insert funName Env.FunEntry{
            Env.formals=map snd formalsTys,
            Env.result=resultTy} venv
          bodyEnv = Map.union venv $ Map.fromList $
            map (\(sym, typ) -> (sym, Env.VarEntry typ)) formalsTys
        in
          case transExp bodyEnv tenv bodyExp of
            Left err -> Left err
            Right ExpTy{exp=_, ty=bodyTy} ->
              if resultTy /= Types.UNIT && resultTy /= bodyTy
              then
                Left SemantError{what="computed type of function body " ++
                                      (show bodyTy) ++ " and annotated type " ++
                                      (show resultTy) ++ " do not match",
                                 at=pos}
              else
                Right (venv', tenv)
  where
    computeFormalTy (A.Field fieldName _ fieldTyp fieldPos) =
      case Map.lookup fieldTyp tenv of
        Nothing -> Left SemantError{what="at parameter " ++ (show fieldName) ++
                                         " in function declaration, unbound type " ++
                                         "variable " ++ (show fieldTyp),
                                    at=fieldPos}
        Just typeTy -> Right (fieldName, typeTy)

transDec venv tenv (A.TypeDec tydecs) =
  let
    stronglyConnComps = typeSCCs tydecs
  in
    case checkForIllegalCycles tydecs stronglyConnComps of
      Left err -> Left err
      Right () ->
        case foldl' step (Right tenv) stronglyConnComps of
          Left err -> Left err
          Right tenv' -> Right (venv, tenv')
        where
          step (Left err) _ = Left err
          step (Right tenv') (CyclicSCC syms) = transCyclicDecls tenv' tydecs syms
          step (Right tenv') (AcyclicSCC sym) = transAcyclicDecl tenv' tydecs sym

transCyclicDecls :: Env.TEnv -> [A.TyDec] -> [Symbol] -> Either SemantError Env.TEnv
transCyclicDecls tenv tydecs syms =
  let
    headers = map (\sym -> (sym, Types.NAME(sym, Nothing))) syms
    bodies = map (\sym -> let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
                          in typ) syms
    headerMap = Map.fromList headers
    tenv' = Map.union tenv headerMap
    maybeTranslatedBodies = sequence $ map (transTy tenv') bodies
  in
    case maybeTranslatedBodies of
      Left err -> Left err
      Right translatedBodies ->
        Right $ tieTheKnot tenv (Map.fromList $ zip syms translatedBodies)
  where
    tieTheKnot :: Env.TEnv -> Map.Map Symbol Types.Ty -> Env.TEnv
    tieTheKnot tenv' bodyMap =
      let
        newTyMap = Map.fromList newTyList
        newTyList = [tieEntry elt | elt <- Map.toList bodyMap]
        tieEntry (sym, Types.RECORD (fieldMap, recordId)) =
          let
            tieFieldEntry :: (Symbol, Types.Ty) -> (Symbol, Types.Ty)
            tieFieldEntry (fieldName, Types.NAME(sym',_)) =
              (fieldName, newTyMap Map.! sym')
            tieFieldEntry (fieldName, typ) =
              (fieldName, typ)
          in
            (sym, Types.RECORD(map tieFieldEntry fieldMap, recordId))
        tieEntry (sym, Types.ARRAY (Types.NAME(sym',_), arrayId)) =
          (sym, Types.ARRAY (newTyMap Map.! sym', arrayId))
        tieEntry (sym, Types.NAME(sym', _)) =
          (sym, newTyMap Map.! sym')
        tieEntry (sym, typ) =
          (sym, typ)
      in
        Map.union newTyMap tenv'

transAcyclicDecl :: Env.TEnv -> [A.TyDec] -> Symbol -> Either SemantError Env.TEnv
transAcyclicDecl tenv tydecs sym =
  let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
  in
    case transTy tenv typ of
      Left err -> Left err
      Right typesTy -> Right $ Map.insert sym typesTy tenv

checkForIllegalCycles :: [A.TyDec] -> [SCC Symbol] -> Either SemantError ()
checkForIllegalCycles tydecs stronglyConnectedComponents =
  let
    cyclicComponents = filter isCyclicSCC stronglyConnectedComponents
    allNameCyclicComponents = filter (allAreName tydecs) cyclicComponents
  in
    case allNameCyclicComponents of
      [] -> Right ()
      (CyclicSCC syms) : _ ->
        let (A.TyDec _ _ posn) = lookupTypeSym (head syms) tydecs
        in
          Left SemantError{
          what="found illegal type declaration cycle (each set of mutually " ++
               "recursive type declarations must pass through a record or array " ++
               "type). Cycle: " ++ (show syms),
          at=posn}
      _ -> error "shouldn't get here: we filtered on isCyclciSCC"

typeSCCs :: [A.TyDec] -> [SCC Symbol]
typeSCCs tydecs =
  let
    typeGraph = calcTypeGraph tydecs
    typeEdges = map (\(sym, _, syms) -> (sym, sym, syms)) typeGraph
  in
    reverse $ stronglyConnComp typeEdges


allAreName :: [A.TyDec] -> SCC Symbol -> Bool
allAreName tydecs (AcyclicSCC sym) = isNameTy tydecs sym
allAreName tydecs (CyclicSCC syms) = all (isNameTy tydecs) syms

lookupTypeSym :: Symbol -> [A.TyDec] -> A.TyDec
lookupTypeSym sym tydecs = case filter (\tydec -> A.tydecName tydec == sym) tydecs of
  [] -> error "shouldn't get here"
  tydec : _ -> tydec

isNameTy :: [A.TyDec] -> Symbol -> Bool
isNameTy tydecs sym =
  case lookupTypeSym sym tydecs of
    (A.TyDec _ (A.NameTy _) _) -> True
    (A.TyDec _ _ _) -> False

calcTypeGraph :: [A.TyDec] -> [(Symbol, A.Pos, [Symbol])]
calcTypeGraph tydecs = fmap calcNeighbors tydecs
  where
    calcNeighbors (A.TyDec name (A.NameTy(name',_)) posn) =
      (name, posn, [name'])
    calcNeighbors (A.TyDec name (A.RecordTy fields) posn) =
      (name, posn, map A.fieldTyp fields)
    calcNeighbors (A.TyDec name (A.ArrayTy(name',_)) posn) =
      (name, posn, [name'])

isCyclicSCC :: SCC vertex -> Bool
isCyclicSCC (CyclicSCC _) = True
isCyclicSCC _ = False

-- TODO need to assign unique type identifiers!!
transTy tenv (A.NameTy(sym, pos)) =
  case Map.lookup sym tenv of
    Nothing -> Left SemantError{what="unbound type variable " ++ (show sym) ++
                                     " in type declaration",
                                at=pos}
    Just typeTy -> Right typeTy
transTy tenv (A.RecordTy fields) =
  case sequence $ map fieldTypeFun fields of
    Left err -> Left err
    Right symAndTys -> Right $ Types.RECORD(symAndTys, 1337)
  where
    fieldTypeFun (A.Field fieldName _ fieldTypSym fieldPos) =
      case Map.lookup fieldTypSym tenv of
        Nothing -> Left SemantError{
          what="unbound type variable used in record field " ++
               (show fieldName),
          at=fieldPos}
        Just typesTy -> Right (fieldName, typesTy)
transTy tenv (A.ArrayTy(arrayEltTypeSym, posn)) =
  case Map.lookup arrayEltTypeSym tenv of
    Nothing -> Left SemantError{what="in array decl, unbound array element type symbol",
                                at=posn}
    Just typeTy -> Right $ Types.ARRAY (typeTy, 1337)
