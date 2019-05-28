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

transProg :: A.Exp -> Either SemantError ExpTy
transProg expr =
  let
    startState = SemantState{valEnv=Env.baseVEnv,
                             typEnv=Env.baseTEnv,
                             canBreak=False,
                             counter=0}
  in
    case transExp startState expr of
      Left err -> Left err
      Right(expTy,_) -> Right expTy

data SemantState = SemantState {valEnv :: Env.VEnv,
                                typEnv :: Env.TEnv,
                                canBreak :: Bool,
                                counter :: Integer}

pushCanBreak :: SemantState -> SemantState
pushCanBreak (SemantState v t _ c) = SemantState{valEnv=v,
                                                 typEnv=t,
                                                 canBreak=True,
                                                 counter=c}

pushCannotBreak :: SemantState -> SemantState
pushCannotBreak (SemantState v t _ c) = SemantState{valEnv=v,
                                                    typEnv=t,
                                                    canBreak=True,
                                                    counter=c}

incrCounter :: SemantState -> (Integer, SemantState)
incrCounter st@(SemantState _ _ _ c) = (c, st{counter=c+1})

-- TODO learn me some monad transformers!
transVar :: SemantState -> A.Var -> Either SemantError (ExpTy, SemantState)
transExp :: SemantState -> A.Exp -> Either SemantError (ExpTy, SemantState)
transDec :: SemantState -> A.Dec -> Either SemantError SemantState
transTy :: SemantState -> A.Ty -> Either SemantError (Types.Ty, SemantState)
transLetDecs :: SemantState -> [A.Dec] -> A.Pos -> Either SemantError SemantState

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

transVar state (A.SimpleVar sym pos) =
  case Map.lookup sym (valEnv state)  of
    Just Env.VarEntry{Env.ty=t} -> Right (ExpTy{exp=emptyExp, ty=t}, state)
    Just (Env.FunEntry _ _) -> Left SemantError{
      what="variable " ++ (show sym) ++ " has no non-function bindings.",
      at=pos}
    Nothing -> Left SemantError{
      what="unbound free variable: " ++ (show sym),
      at=pos}
transVar state (A.FieldVar var sym pos) = do
  (ExpTy{exp=_, ty=varTy}, state') <- transVar state var
  case varTy of
    r@(Types.RECORD(sym2ty, _)) -> case lookup sym sym2ty of
                                 Just t -> return (ExpTy{exp=emptyExp, ty=t}, state')
                                 Nothing -> Left SemantError{
                                   what="in field expr, record type " ++
                                        (show r) ++ " has no " ++ (show sym) ++ " field",
                                   at=pos}
    t@(_) -> Left SemantError{
      what="in field expr, only record types have fields. type=" ++ (show t),
      at=pos}
transVar state (A.SubscriptVar var expr pos) = do
  (ExpTy{exp=_, ty=varTy}, state') <- transVar state var
  case varTy of
    Types.ARRAY(varEltTy, _) -> do
      (ExpTy{exp=_, ty=expTy}, state'') <- transExp state' expr
      case expTy of
        Types.INT -> return (ExpTy{exp=emptyExp, ty=varEltTy}, state'')
        nonIntTy@(_) -> Left SemantError{
          what="in subscript expr, subscript type is not an INT, is an " ++ (show nonIntTy),
          at=pos}
    nonArrayTy@(_) -> Left SemantError{
      what="in subscript expr, only arrays may be subscripted -- attempting to subscript type=" ++
           (show nonArrayTy),
      at=pos}

transExp state (A.VarExp var) = transVar state var
transExp state A.NilExp = Right (ExpTy{exp=emptyExp, ty=Types.NIL}, state)
transExp state (A.IntExp _) = Right (ExpTy{exp=emptyExp, ty=Types.INT}, state)
transExp state (A.StringExp _) = Right (ExpTy{exp=emptyExp, ty=Types.STRING}, state)
transExp state (A.CallExp funcSym argExps pos) =
  case Map.lookup funcSym (valEnv state) of
    Just (Env.FunEntry formalsTys resultTy) ->
      case
        foldl'
        (\acc argExp -> case acc of
            Left err -> Left err
            Right(paramExpTys,state') ->
              case transExp state' argExp of
                Left err -> Left err
                Right(expTy,state'') -> Right (paramExpTys ++ [expTy],state'')
        )
        (Right ([], state))
        argExps
      of
        Left err -> Left err
        Right (paramExpTys, state') ->
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
                [] -> Right (ExpTy{exp=emptyExp, ty=resultTy}, state')
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
transExp state (A.OpExp leftExp op rightExp pos) =
  do
    (ExpTy{exp=_, ty=tyleft}, state') <- transExp state leftExp
    (ExpTy{exp=_, ty=tyright}, state'') <- transExp state' rightExp
    if isArith op then
      let maybeError = do
            checkInt tyleft (Just "in left hand operand")
            checkInt tyright (Just "in right hand operand") in
        case maybeError of
          Left err -> Left SemantError{
            what="In OpExp, " ++ err,
            at=pos}
          Right _ -> return (ExpTy{exp=emptyExp, ty=Types.INT}, state'')
      else
      if isCmp op
      then
        let cmpReturn = return (ExpTy{exp=emptyExp, ty=Types.INT}, state'') in
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
transExp state (A.RecordExp fieldSymExpPosns typSym pos) =
  case Map.lookup typSym (typEnv state) of
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
              case
                foldl'
                (\acc (_,expr,_) -> case acc of
                    Left err -> Left err
                    Right(fieldExpTys,state') ->
                      case transExp state' expr of
                        Left err -> Left err
                        Right (fieldExpTy,state'') ->
                          Right (fieldExpTys ++ [fieldExpTy],state'')
                )
                (Right ([], state))
                fieldSymExpPosns
              of
                Left err -> Left err
                Right (actualFieldExpTys, state') ->
                  let
                    expectedFieldTys = map (actualTy . snd) sym2ty
                    actualFieldTys = map ty actualFieldExpTys
                    fieldPosns = map (\(_,_,fieldPos) -> fieldPos) fieldSymExpPosns
                  in
                    case filter (\(_,expectedTy,actualTy',_) -> expectedTy /= actualTy')
                         (zip4 expectedSyms expectedFieldTys actualFieldTys fieldPosns) of
                      [] -> Right (ExpTy{exp=emptyExp, ty=recordTy}, state')
                      ((sym,expectedTy,actualTy',fieldPos):_) ->
                        Left SemantError{what="in record exp, field " ++ (show sym) ++
                                              " should have type " ++ (show expectedTy) ++
                                              " but has type " ++ (show actualTy'),
                                         at=fieldPos}
        t@(_) -> Left SemantError{
          what="only record types may appear as the symbol in a record instance " ++
               "definition. Found type=" ++ (show t),
          at=pos}
transExp state (A.SeqExp expAndPosns) =
  case
    foldl'
    (\acc (expr,_) -> case acc of
                   Left err -> Left err
                   Right(expTys,state') ->
                     case transExp state' expr of
                       Left err -> Left err
                       Right(expTy,state'') -> Right(expTys ++ [expTy],state''))
    (Right ([],state))
    expAndPosns
  of
    Left err -> Left err
    Right ([],state') -> Right (ExpTy{exp=emptyExp, ty=Types.UNIT},state')
    Right (expTys, state') -> Right (last expTys, state')
transExp state (A.AssignExp var expr pos) =
  do
    (ExpTy{exp=_, ty=varTy}, state') <- transVar state var
    (ExpTy{exp=_, ty=exprTy}, state'') <- transExp state' expr
    if varTy == exprTy then
      return (ExpTy{exp=emptyExp, ty=Types.UNIT}, state'')
      else
      Left SemantError{what="in assignExp, variable has type " ++ (show varTy) ++
                            " but assign target has type " ++ (show exprTy),
                       at=pos}
transExp state
  (A.IfExp testExpr thenExpr maybeElseExpr pos) =
  do
    (testExpTy, state') <- transExp state testExpr
    (thenExpTy, state'') <- transExp state' thenExpr
    let maybeElseExpTy = fmap (transExp state'') maybeElseExpr in
      if (ty testExpTy) /= Types.INT
      then
        Left SemantError{what="in ifExp, the test expression must be integral: " ++
                              "found type=" ++ (show $ ty testExpTy),
                         at=pos}
      else
        case maybeElseExpTy of
          Nothing -> return (thenExpTy, state'')
          Just elseExpTyEither -> do
            (elseExpTy, state''') <- elseExpTyEither
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
                return (ExpTy{exp=emptyExp, ty=thenTy}, state''')
transExp state (A.WhileExp testExp bodyExp pos) =
  do
    (ExpTy{exp=_, ty=testTy}, state') <- transExp state testExp
    (_, state'') <- transExp (state'{canBreak=True}) bodyExp
    if testTy /= Types.INT
    then
      Left SemantError{what="in whileExp, the test expression must be integral: " ++
                            "found type=" ++ (show testTy),
                       at=pos}
      else
      return (ExpTy{exp=emptyExp, ty=Types.UNIT}, state'')
transExp st@(SemantState _ _ True _) (A.BreakExp _) =
    Right (ExpTy{exp=emptyExp, ty=Types.UNIT}, st)
transExp (SemantState _ _ False _) (A.BreakExp pos) =
    Left SemantError{what="break expression not enclosed in a while or for",
                     at=pos}
transExp state (A.ArrayExp arrayTySym sizeExp initExp pos) =
  case Map.lookup arrayTySym (typEnv state) of
    Nothing -> Left SemantError{
      what="unbound free type variable " ++ (show arrayTySym),
      at=pos}
    Just maybeArrayTy ->
      case maybeArrayTy of
        arrayTy@(Types.ARRAY(arrayEltTy,_)) -> do
          (ExpTy{exp=_, ty=sizeTy}, state') <- transExp state sizeExp
          (ExpTy{exp=_, ty=initTy}, state'') <- transExp state' initExp
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
              return (ExpTy{exp=emptyExp, ty=arrayTy}, state'')
        t@(_) -> Left SemantError{
          what="only array types may appear as the symbol in an array instance " ++
               "definition. Found type=" ++ (show t),
          at=pos}
transExp state (A.ForExp forVar _ loExp hiExp body pos) =
  let
    bodyVEnv = Map.insert forVar Env.VarEntry{Env.ty=Types.INT} (valEnv state)
    state' = state{valEnv=bodyVEnv}
  in
    do
      (ExpTy{exp=_, ty=loTy}, state'') <- transExp state' loExp
      (ExpTy{exp=_, ty=hiTy}, state''') <- transExp state'' hiExp
      (ExpTy{exp=_, ty=bodyTy}, state'''') <- transExp (state'''{canBreak=True}) body
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
            _ -> return (ExpTy{exp=emptyExp, ty=Types.UNIT}, state'''')
transExp state (A.LetExp decs bodyExp letPos) = do
  state' <- transLetDecs state decs letPos
  transExp state' bodyExp

transLetDecs state decls letPos =
  case checkDeclNamesDistinctInLet decls letPos of
    Left err -> Left err
    _ -> foldl' step (Right state) decls
      where
        step (Left err) _ = Left err
        step (Right state') decl =
          transDec state' decl

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

transDec state (A.VarDec name _ maybeTypenameAndPos initExp posn) =
  let maybeTypeAnnotation =
        join $ fmap (\(typename,_) -> Map.lookup typename (typEnv state)) maybeTypenameAndPos in
    case transExp state initExp of
      Left err -> Left err
      Right (ExpTy{exp=_, ty=actualInitTy}, state') ->
        if actualInitTy == Types.NIL
        then
          case maybeTypeAnnotation of
            Just recTy@(Types.RECORD _) ->
              Right state'{valEnv=Map.insert name (Env.VarEntry recTy) (valEnv state')}
            _ -> Left SemantError{
              what="nil expression declarations must be constrained by a RECORD type",
              at=posn}
        else
          let result = Right state'{
                valEnv=Map.insert name (Env.VarEntry actualInitTy) (valEnv state')}
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
transDec state (A.FunctionDec fundecs) =
  let
    resultMaybeTys =
      map (\fundec ->
              (join $ fmap (\(typename,_) ->
                              Map.lookup
                              typename
                              (typEnv state))
                (A.result fundec)))
      fundecs
    maybeFormalsTys =
      sequence $ map (\fundec -> sequence $
                                 fmap
                                 computeFormalTy
                                 (A.params fundec)) fundecs
  in
    case maybeFormalsTys of
      Left err -> Left err
      Right formalsTys ->
        let
          resultTys = map resultTyFun resultMaybeTys
          headerVEnv =
            foldl'
            (\venv (fundec,paramTys,resultTy) ->
               Map.insert
               (A.fundecName fundec)
               Env.FunEntry{Env.formals=map snd paramTys,
                            Env.result=resultTy}
               venv)
            (valEnv state)
            (zip3 fundecs formalsTys resultTys)
        in
          foldl'
          transBody
          (Right state{valEnv=headerVEnv})
          (zip3 fundecs formalsTys resultTys)
  where
    computeFormalTy (A.Field fieldName _ fieldTyp fieldPos) =
      case Map.lookup fieldTyp (typEnv state) of
        Nothing -> Left SemantError{what="at parameter " ++ (show fieldName) ++
                                         " in function declaration, unbound type " ++
                                         "variable " ++ (show fieldTyp),
                                    at=fieldPos}
        Just typeTy -> Right (fieldName, typeTy)
    resultTyFun maybeResultTy = case maybeResultTy of
                                  Nothing -> Types.UNIT
                                  Just typ -> typ
    transBody (Left err) _ = Left err
    transBody (Right state') (fundec,formalsTys,resultTy) =
      let
        bodyVEnv = Map.union (valEnv state') $ Map.fromList $
                   map (\(sym,typ) -> (sym, Env.VarEntry typ)) formalsTys
      in
        case transExp state'{valEnv=bodyVEnv} (A.funBody fundec) of
          Left err -> Left err
          Right (ExpTy{exp=_, ty=bodyTy}, state'') ->
            if resultTy /= Types.UNIT && resultTy /= bodyTy
            then
              Left SemantError{what="computed type of function body " ++
                                    (show bodyTy) ++ " and annotated type " ++
                                    (show resultTy) ++ " do not match",
                               at=(A.funPos fundec)}
            else
              Right state''
transDec state (A.TypeDec tydecs) =
  let
    stronglyConnComps = typeSCCs tydecs
  in
    case checkForIllegalCycles tydecs stronglyConnComps of
      Left err -> Left err
      Right () ->
        foldl' step (Right state) stronglyConnComps
        where
          step (Left err) _ = Left err
          step (Right state') (CyclicSCC syms) = transCyclicDecls state' tydecs syms
          step (Right state') (AcyclicSCC sym) = transAcyclicDecl state' tydecs sym

transCyclicDecls :: SemantState -> [A.TyDec] -> [Symbol] -> Either SemantError SemantState
transCyclicDecls state tydecs syms =
  let
    tenv = typEnv state
    headers = map (\sym -> (sym, Types.NAME(sym, Nothing))) syms
    bodies = map (\sym -> let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
                          in typ) syms
    headerMap = Map.fromList headers
    tenv' = Map.union tenv headerMap
    state' = state{typEnv=tenv'}
    maybeTranslatedBodies =
      foldl'
      (\acc typ -> case acc of
                     Left err -> Left err
                     Right (translatedBodies, state'') ->
                       case transTy state'' typ of
                         Left err -> Left err
                         Right (typeTy,state''') ->
                           Right (translatedBodies ++ [typeTy], state'''))
      (Right ([], state'))
      bodies
  in
    case maybeTranslatedBodies of
      Left err -> Left err
      Right (translatedBodies, state'') ->
        let
          tenv'' = tieTheKnot (typEnv state'') (Map.fromList $ zip syms translatedBodies)
        in
          Right state''{typEnv=tenv''}
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

transAcyclicDecl :: SemantState -> [A.TyDec] -> Symbol -> Either SemantError SemantState
transAcyclicDecl state tydecs sym =
  let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
  in
    case transTy state typ of
      Left err -> Left err
      Right (typesTy, state') -> Right
        state'{typEnv=Map.insert sym typesTy (typEnv state)}

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

transTy state (A.NameTy(sym, pos)) =
  case Map.lookup sym (typEnv state) of
    Nothing -> Left SemantError{what="unbound type variable " ++ (show sym) ++
                                     " in type declaration",
                                at=pos}
    Just typeTy -> Right (typeTy,state)
transTy state (A.RecordTy fields) =
  case sequence $ map fieldTypeFun fields of
    Left err -> Left err
    Right symAndTys ->
      let
        (typeId, state') = incrCounter state
      in
        Right (Types.RECORD(symAndTys, typeId), state')
  where
    fieldTypeFun (A.Field fieldName _ fieldTypSym fieldPos) =
      case Map.lookup fieldTypSym (typEnv state) of
        Nothing -> Left SemantError{
          what="unbound type variable used in record field " ++
               (show fieldName),
          at=fieldPos}
        Just typesTy -> Right (fieldName, typesTy)
transTy state (A.ArrayTy(arrayEltTypeSym, posn)) =
  case Map.lookup arrayEltTypeSym (typEnv state) of
    Nothing -> Left SemantError{what="in array decl, unbound array element type symbol",
                                at=posn}
    Just typeTy ->
      let
        (typeId,state') = incrCounter state
      in
        Right (Types.ARRAY(typeTy, typeId), state')
