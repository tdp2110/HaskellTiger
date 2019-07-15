module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Frame
import qualified Translate as Translate
import qualified Types as Types
import Symbol
import FindEscape (escapeExp)
import qualified Temp

import Control.Monad.Trans.Class
import Control.Monad (join, foldM)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, get, put, evalStateT, runStateT)
import Data.Functor.Identity
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
    startState = SemantState{ level=outermost
                            , canBreak=False
                            , counter'=0
                            , generator=Temp.newGen}
    env = SemantEnv{ venv'=Env.baseVEnv
                   , tenv2=Env.baseTEnv }
  in
    evalTransT startState env $ transExp $ escapeExp expr

transVar :: A.Var -> Translator ExpTy
transExp :: A.Exp -> Translator ExpTy
transTy :: A.Ty -> Translator Types.Ty
transLetDecs :: [A.Dec] -> A.Pos -> Translator SemantEnv

transDec :: A.Dec -> Translator SemantEnv

type Level = Translate.X64Level
type Translate = Translate.X64Translate
type Generator = Temp.Generator
outermost :: Level
outermost = Translate.X64Outermost
newLevelFn :: (Level, Temp.Label, [Frame.EscapesOrNot]) -> Temp.Generator
  -> (Temp.Generator, Level)
newLevelFn = Translate.x64NewLevel

data SemantEnv = SemantEnv {venv'::Env.VEnv, tenv2 :: Env.TEnv}
data SemantState = SemantState { level :: Level
                               , canBreak :: Bool
                               , counter' :: Integer
                               , generator :: Generator }

type Translator = StateT SemantState (ReaderT SemantEnv (ExceptT SemantError Identity))

throwT :: A.Pos -> String -> Translator a
throwT posn str = throwErr SemantError{what=str, at=posn}

throwErr :: SemantError -> Translator a
throwErr err = (lift . lift . throwE) err

lookupT :: A.Pos -> (SemantEnv -> Map.Map Symbol a) -> Symbol -> Translator a
lookupT posn f sym = do
  env <- lift $ asks f
  case Map.lookup sym env of
    Nothing -> throwT posn $ "unbound variable " ++ (show sym)
    Just x -> return x

runTransT :: SemantState -> SemantEnv -> Translator a -> Either SemantError (a, SemantState)
runTransT st env =
  runIdentity . runExceptT . flip runReaderT env . flip runStateT st

evalTransT :: SemantState -> SemantEnv -> Translator a -> Either SemantError a
evalTransT st env =
  runIdentity . runExceptT . flip runReaderT env . flip evalStateT st

nextId :: Translator Integer
nextId = do
  st@(SemantState _ _ currId _) <- get
  put st{counter'=currId + 1}
  return currId

newLevel :: [Frame.EscapesOrNot] -> Translator Level
newLevel escapes = do
  st@(SemantState parentLev _ _ gen) <- get
  let
    (nextLab, gen') = Temp.newlabel gen
    (gen'', lev') = newLevelFn (parentLev, nextLab, escapes) gen'
    in do
    put st{generator=gen''}
    return lev'

transTy (A.NameTy(sym, posn)) = do
  typ <- lookupT posn tenv2 sym
  return typ
transTy (A.RecordTy fields) =  do
  symAndTys <- mapM mapFunc fields
  typeId <- nextId
  return $ Types.RECORD(symAndTys, typeId)
  where
    mapFunc (A.Field fieldName _ fieldTypSym fieldPos) = do
      typ <- lookupT fieldPos tenv2 fieldTypSym
      return (fieldName, typ)

transTy (A.ArrayTy(arrayEltTypeSym, posn)) = do
  typ <- lookupT posn tenv2 arrayEltTypeSym
  typeId <- nextId
  return $ Types.ARRAY(typ, typeId)

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

transVar (A.SimpleVar sym pos) = do
  val <- lookupT pos venv' sym
  case val of
    Env.VarEntry{Env.ty=t} -> return ExpTy{exp=emptyExp, ty=t}
    (Env.FunEntry _ _ _ _) -> throwT pos ("variable " ++ (show sym) ++
                                      " has no non-function bindings.")
transVar (A.FieldVar var sym pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar var
  case varTy of
    r@(Types.RECORD(sym2ty, _)) ->
      case lookup sym sym2ty of
        Just t -> return ExpTy{exp=emptyExp, ty=t}
        Nothing -> throwT pos ("in field expr, record type " ++
                               (show r) ++ " has no " ++ (show sym) ++
                               " field")
    t@(_) -> throwT pos ("in field expr, only record types have fields. type=" ++
                         (show t))
transVar (A.SubscriptVar var expr pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar var
  ExpTy{exp=_, ty=expTy} <- transExp expr
  case varTy of
    Types.ARRAY(varEltTy, _) ->
      case expTy of
        Types.INT -> return ExpTy{exp=emptyExp, ty=varEltTy}
        nonIntTy@(_) -> throwT pos ("in subscript expr, subscript type " ++
                                     "is not an INT, is an " ++ (show nonIntTy))
    nonArrayTy@(_) -> throwT pos ("in subscript expr, only arrays may " ++
                                  "be subscripted -- attempting to subscript type=" ++
                                  (show nonArrayTy))

transExp (A.VarExp var) = do
  st <- get
  env <- lift ask
  case  runTransT st env (transVar var) of
    Left err -> throwErr err
    Right (res, newState) -> do
      put newState
      return res
transExp A.NilExp = do
  return ExpTy{exp=emptyExp, ty=Types.NIL}
transExp (A.IntExp _) = do
  return ExpTy{exp=emptyExp, ty=Types.INT}
transExp (A.StringExp _) = do
  return ExpTy{exp=emptyExp, ty=Types.STRING}
transExp (A.CallExp funcSym argExps pos) = do
  funEntry <- lookupT pos venv' funcSym
  case funEntry of
    (Env.FunEntry _ _ formalsTys resultTy) -> do
      paramExpTys <- mapM transExp argExps
      let paramTys = map ty paramExpTys in
        if (length formalsTys) /= (length paramTys)
        then
          throwT pos ("function " ++ (show funcSym) ++
                      " expects " ++ (show $ length formalsTys) ++
                      " parameters but was passed " ++ (show $ length paramTys))
        else
          case filter (\(ty1, ty2, _) -> ty1 /= ty2)
               (zip3 formalsTys paramTys [0 :: Integer ..]) of
            [] -> return ExpTy{exp=emptyExp, ty=resultTy}
            ((formalTy, paramTy, ix):_) ->
              throwT pos ("parameter " ++ (show ix) ++ " of func " ++ (show funcSym) ++
                          " requires type " ++ (show formalTy) ++
                          " but was passed a value of type " ++ (show paramTy))
    (Env.VarEntry t) -> throwT pos ("only functions are callable: found type " ++
                                   (show t))
transExp (A.OpExp leftExp op rightExp pos) = do
  ExpTy{exp=_, ty=tyleft} <- transExp leftExp
  ExpTy{exp=_, ty=tyright} <- transExp rightExp
  if isArith op then
    let maybeError = do
          checkInt tyleft (Just "in left hand operand")
          checkInt tyright (Just "in right hand operand")
    in
      case maybeError of
        Left err -> throwT pos ("In OpExp, " ++ err)
        Right _ -> return ExpTy{exp=emptyExp, ty=Types.INT}
    else
    if isCmp op then
      let
        res = return ExpTy{exp=emptyExp, ty=Types.INT}
      in
        case (tyleft, tyright) of
          (Types.INT, Types.INT) -> res
          (Types.STRING, Types.STRING) -> res
          (r1@(Types.RECORD _), r2@(Types.RECORD _)) ->
            if r1 == r2 then res
              else
              throwT pos "only identical record types may be compared"
          (arr1@(Types.ARRAY _), arr2@(Types.ARRAY _)) ->
            if arr1 == arr2 then res
            else
              throwT pos "only identical array types may be compared"
          _ -> throwT pos ("incomparable types " ++ (show tyleft) ++
                           " and " ++ (show tyright))
    else undefined
transExp (A.RecordExp fieldSymExpPosns typSym pos) = do
  maybeRecordTy <- lookupT pos tenv2 typSym
  case maybeRecordTy of
    recordTy@(Types.RECORD(sym2ty, _)) ->
      let
        expectedSyms = map fst sym2ty
        actualSyms = map (\(sym,_,_) -> sym) fieldSymExpPosns
      in
        if actualSyms /= expectedSyms
        then
          throwT pos ("incompatible field names: expected " ++
                       (show expectedSyms) ++ " but record expression has " ++
                       (show actualSyms))
        else do
          actualFieldExpTys <- mapM (\(_,expr,_) -> transExp expr) fieldSymExpPosns
          let
            expectedFieldTys = map snd sym2ty
            actualFieldTys = map ty actualFieldExpTys
            fieldPosns = map (\(_,_,fieldPos) -> fieldPos) fieldSymExpPosns
            in
            case filter (\(_,expectedTy,actualTy,_) -> expectedTy /= actualTy)
                 (zip4 expectedSyms expectedFieldTys actualFieldTys fieldPosns)
            of
              [] -> return ExpTy{exp=emptyExp, ty=recordTy}
              ((sym,expectedTy,actualTy,fieldPos):_) ->
                throwT fieldPos ("in record exp, field " ++ (show sym) ++
                                 " should have type " ++ (show expectedTy) ++
                                 " but has type " ++ (show actualTy))
    t@(_) ->
          throwT pos ("only record types may appear as the symbol in a " ++
                      "record instance " ++
                      "definition. Found type=" ++ (show t))
transExp (A.SeqExp expAndPosns) = do
  expTys <- mapM
    (\(expr,_) -> transExp expr)
    expAndPosns
  case expTys of
    [] -> return ExpTy{exp=emptyExp, ty=Types.UNIT}
    _ -> return $ last expTys
transExp (A.AssignExp var expr pos) = do
  ExpTy{exp=_, ty=varTy} <- transVar var
  ExpTy{exp=_, ty=exprTy} <- transExp expr
  if varTy == exprTy then
    return ExpTy{exp=emptyExp, ty=Types.UNIT}
    else
    throwT pos ("in assignExp, variable has type " ++ (show varTy) ++
                " but assign target has type " ++ (show exprTy))
transExp (A.IfExp testExpr thenExpr maybeElseExpr pos) = do
  testExpTy <- transExp testExpr
  thenExpTy <- transExp thenExpr
  let
    maybeElseExpTy = fmap transExp maybeElseExpr in
    if (ty testExpTy) /= Types.INT then
      throwT pos ("in ifExp, the test expression must be integral: " ++
                  "found type=" ++ (show $ ty testExpTy))
    else
      case maybeElseExpTy of
        Nothing -> return thenExpTy
        Just elseExpTyM -> do
          elseExpTy <- elseExpTyM
          let
            thenTy = ty thenExpTy
            elseTy = ty elseExpTy
            in
            if thenTy /= elseTy then
              throwT pos ("in ifExp, thenExp and elseExp must have " ++
                          "the same type: found " ++ (show thenTy) ++
                          " and " ++ (show elseTy) ++
                          ", respectfully")
            else
              return ExpTy{exp=emptyExp, ty=thenTy}
transExp (A.WhileExp testExp bodyExp pos) = do
  ExpTy{exp=_, ty=testTy} <- transExp testExp
  st <- get
  put st{canBreak=True}
  ExpTy{exp=_, ty=bodyTy} <- transExp bodyExp
  put st{canBreak=False}
  if testTy /= Types.INT then
    throwT pos ("in whileExp, the test expression must be integral: " ++
                "found type=" ++ (show testTy))
    else if bodyTy /= Types.UNIT then
    throwT pos ("in a whileExp, the body expression must yield no value: " ++
                "found type=" ++ (show bodyTy)) -- TODO add a test for me!
    else
    return ExpTy{exp=emptyExp, ty=Types.UNIT}
transExp (A.BreakExp pos) = do
  (SemantState _ canBreak' _ _) <- get
  if canBreak' then
    return ExpTy{exp=emptyExp, ty=Types.UNIT}
    else
    throwT pos "break expression not enclosed in a while or for"
transExp (A.ArrayExp arrayTySym sizeExp initExp pos) = do
  maybeArrayTy <- lookupT pos tenv2 arrayTySym
  case maybeArrayTy of
    arrayTy@(Types.ARRAY(arrayEltTy,_)) -> do
      ExpTy{exp=_, ty=sizeTy} <- transExp sizeExp
      ExpTy{exp=_, ty=initTy} <- transExp initExp
      if sizeTy /= Types.INT then
        throwT pos ("in ArrayExp, sizeExp must be an integer. " ++
                    "Found type=" ++ (show sizeTy))
        else if initTy /= arrayEltTy then
        throwT pos ("in ArrayExp, initExp has actual type " ++
                    (show initTy) ++ ", when it must have " ++
                    (show arrayEltTy))
        else
        return ExpTy{exp=emptyExp, ty=arrayTy}
    t@(_) ->
          throwT pos ("only array types may appear as the symbol in an " ++
                      "array instance " ++
                      "definition. Found type=" ++ (show t))
transExp (A.ForExp forVar _ loExp hiExp body pos) = do
  ExpTy{exp=_, ty=loTy} <- transExp loExp
  ExpTy{exp=_, ty=hiTy} <- transExp hiExp
  st <- get
  env <- lift ask
  let
    bodyVEnv = Map.insert forVar Env.VarEntry{Env.ty=Types.INT} (venv' env)
    bodyEnv = env{venv'=bodyVEnv}
    in
    case runTransT st{canBreak=True} bodyEnv (transExp body) of
      Left err -> throwErr err
      Right (ExpTy{exp=_, ty=bodyTy}, st') -> do
        put st'{canBreak=False}
        if (loTy /= Types.INT) || (hiTy /= Types.INT) then
          throwT pos "only integer expressions may appear as bounds in a ForExp"
          else if bodyTy /= Types.UNIT then
          throwT pos "the body of a ForExp must yield no value"
          else
          case checkForVarNotAssigned forVar body of
            Left err -> throwErr err
            _ -> return ExpTy{exp=emptyExp, ty=Types.UNIT}
transExp (A.LetExp decs bodyExp letPos) = do
  env <- lift ask
  st <- get
  case runTransT st env (transLetDecs decs letPos) of
    Left err -> throwErr err
    Right (bodyEnv, st') ->
      case
        runTransT st' bodyEnv (transExp bodyExp)
      of
        Left err -> throwErr err
        Right (res, st'') -> do
          put st''
          return res

transLetDecs decls letPos = do
  env <- lift ask
  case checkDeclNamesDistinctInLet decls letPos of
    Left err -> throwErr err
    Right () ->
      foldM step env decls
  where
    step envAcc decl = do
      st <- get
      case runTransT st envAcc (transDec decl) of
        Left err -> throwErr err
        Right (newEnv, newState) -> do
          put newState
          return newEnv

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

transDec (A.VarDec name _ maybeTypenameAndPos initExp posn) = do
  maybeTypeAnnotation <- mapM
                         (\(typename,_) -> lookupT posn tenv2 typename)
                         maybeTypenameAndPos
  ExpTy{exp=_, ty=actualInitTy} <- transExp initExp
  env <- lift ask
  if actualInitTy == Types.NIL then
    case maybeTypeAnnotation of
      Just recTy@(Types.RECORD _) ->
        return env{venv'=Map.insert name (Env.VarEntry recTy) (venv' env)}
      _ -> throwT posn ("nil expression declarations must be " ++
                        "constrained by a RECORD type")
    else
    let
      venv'' = Map.insert name (Env.VarEntry actualInitTy) (venv' env)
      result = return env{venv'=venv''}
    in
      case maybeTypeAnnotation of
        Just typeAnnotation ->
          if typeAnnotation /= actualInitTy then
            throwT posn ("mismatch in type annotation and computed " ++
                         "type in varDecl: " ++
                         "type annotation " ++ (show typeAnnotation) ++
                         ", computed type " ++ (show actualInitTy))
          else result
        Nothing -> result
transDec (A.FunctionDec fundecs) = do
  env <- lift ask
  let
    resultMaybeTys =
      map (\fundec ->
              (join $ fmap (\(typename,_) ->
                              Map.lookup
                              typename
                              $ tenv2 env)
                $ A.result fundec))
      fundecs
    maybeFormalsTys =
      sequence $ map (\fundec -> sequence $
                                 fmap
                                 (computeFormalTy env)
                                 (A.params fundec)) fundecs
    in
    case maybeFormalsTys of
      Left err -> throwErr err
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
            (venv' env)
            (zip3 fundecs formalsTys resultTys)
          headerEnv = env{venv'=headerVEnv}
        in
          foldM
          transBody
          headerEnv
          (zip3 fundecs formalsTys resultTys)
  where
    computeFormalTy env (A.Field fieldName _ fieldTyp fieldPos) =
      case Map.lookup fieldTyp (tenv2 env) of
        Nothing -> Left SemantError{what="at parameter " ++ (show fieldName) ++
                                         " in function declaration, unbound type " ++
                                         "variable " ++ (show fieldTyp),
                                    at=fieldPos}
        Just typeTy -> Right (fieldName, typeTy)
    resultTyFun maybeResultTy = case maybeResultTy of
                                  Nothing -> Types.UNIT
                                  Just typ -> typ
    transBody env (fundec,formalsTys,resultTy) = do
      st <- get
      let
        venv = venv' env
        bodyVEnv = Map.union venv $ Map.fromList $
          map (\(sym,typ) -> (sym, Env.VarEntry typ)) formalsTys
        bodyEnv = env{venv'=bodyVEnv}
        in
        case
          runTransT st bodyEnv (transExp $ A.funBody fundec)
        of
          Left err -> throwErr err
          Right (ExpTy{exp=_, ty=bodyTy}, state') ->
            if resultTy /= Types.UNIT && resultTy /= bodyTy
            then
              throwT (A.funPos fundec) ("computed type of function body " ++
                                        (show bodyTy) ++ " and annotated type " ++
                                        (show resultTy) ++ " do not match")
            else do
              put state'
              return env
transDec (A.TypeDec tydecs) =
  let
    stronglyConnComps = typeSCCs tydecs
  in
    case checkForIllegalCycles tydecs stronglyConnComps of
      Left err -> throwErr err
      Right () -> do
        env <- lift ask
        let
          step env' (CyclicSCC syms) = transCyclicDecls env' tydecs syms
          step env' (AcyclicSCC sym) = transAcyclicDecl env' tydecs sym
          in
          foldM step env stronglyConnComps

transCyclicDecls :: SemantEnv -> [A.TyDec] -> [Symbol] -> Translator SemantEnv
transCyclicDecls env tydecs syms = do
  state <- get
  let
    tenv = tenv2 env
    headers = map (\sym -> (sym, Types.NAME(sym, Nothing))) syms
    bodies = map (\sym -> let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
                          in typ) syms
    headerMap = Map.fromList headers
    tenv' = Map.union tenv headerMap
    env' = env{tenv2=tenv'}
    maybeTranslatedBodiesM =
      foldM
      (\acc typ -> do
          state' <- get
          case
            runTransT state' env' (transTy typ)
            of
            Left err -> throwErr err
            Right (typeTy, newState) -> do
              put newState
              return $ acc ++ [typeTy]
      )
      []
      bodies
    in
    case
      runTransT state env' maybeTranslatedBodiesM
    of
      Left err -> throwErr err
      Right (translatedBodies, state') -> do
        put state'
        return $ env{tenv2=tieTheKnot tenv'
                           (Map.fromList $ zip syms translatedBodies)}
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

transAcyclicDecl :: SemantEnv -> [A.TyDec] -> Symbol -> Translator SemantEnv
transAcyclicDecl env tydecs sym = do
  state <- get
  let
    (A.TyDec _ typ _) = lookupTypeSym sym tydecs
    in
    case runTransT
         state
         env
         (transTy typ) of
      Left err -> throwErr err
      Right (typesTy, state') -> do
        put state'
        return $ env{tenv2=Map.insert sym typesTy (tenv2 env)}

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
