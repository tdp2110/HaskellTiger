module Semant where

import qualified Absyn as A
import qualified Env as Env
import qualified Frame
import qualified X64Frame
import qualified Translate as Translate
import qualified Types as Types
import Symbol
import FindEscape (escapeExp)
import qualified Temp
import qualified Tree

import Control.Monad.Trans.Class
import Control.Monad (join, foldM)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State (StateT, get, put, evalStateT, runStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.Either
import Data.List
import Data.Graph
import Prelude hiding (exp)

data SemantError = SemantError{what :: String, at :: A.Pos} deriving (Eq)
instance Show SemantError where
  show (SemantError err pos) = "semantic issue at " ++ (show pos) ++ ": " ++ (show err)

data ExpTy = ExpTy{exp :: Translate.Exp, ty :: Types.Ty } deriving (Show)

transProg :: A.Exp -> Either SemantError (ExpTy, FragList)
transProg expr =
  let
    gen = Temp.newGen
    (gen', mainLevel) = newLevelFn (outermost, Temp.Label $ Symbol "main", []) gen
    startState = SemantState{ level=mainLevel
                            , breakTarget=Nothing
                            , counter'=0
                            , generator=gen'}
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
type Access = Translate.X64Access
type Frag = X64Frame.Frag
formalAccesses :: Level -> [Access]
formalAccesses = Translate.x64TranslateFormals
outermost :: Level
outermost = Translate.X64Outermost
newLevelFn :: (Level, Temp.Label, [Frame.EscapesOrNot]) -> Temp.Generator
  -> (Temp.Generator, Level)
newLevelFn = Translate.x64NewLevel
allocLocalFn :: Level -> Temp.Generator -> Frame.EscapesOrNot
  -> (Temp.Generator, Level, Access)
allocLocalFn = Translate.x64AllocLocal

toEscape :: Bool -> Frame.EscapesOrNot
toEscape True = Frame.Escapes
toEscape _ = Frame.NoEscape

data SemantEnv = SemantEnv {venv'::Env.VEnv, tenv2 :: Env.TEnv}
data SemantState = SemantState { level :: Level
                               , breakTarget :: Maybe Temp.Label
                               , counter' :: Integer
                               , generator :: Generator }

type FragList = DList.DList Frag

type Translator = StateT SemantState
  (WriterT FragList
   (ReaderT SemantEnv (ExceptT SemantError Identity)))

throwT :: A.Pos -> String -> Translator a
throwT posn str = throwErr SemantError{what=str, at=posn}

throwErr :: SemantError -> Translator a
throwErr = lift . lift . lift . throwE

pushFrag :: Frag -> Translator ()
pushFrag = lift . tell . DList.singleton

askEnv :: Translator SemantEnv
askEnv = (lift . lift) ask

lookupT :: A.Pos -> (SemantEnv -> Map.Map Symbol a) -> Symbol -> Translator a
lookupT posn f sym = do
  env <- (lift . lift) $ asks f
  case Map.lookup sym env of
    Nothing -> throwT posn $ "unbound variable " ++ (show sym)
    Just x -> return x

runTransT :: SemantState -> SemantEnv -> Translator a
  -> Either SemantError ((a, SemantState), FragList)
runTransT st env =
  runIdentity . runExceptT . flip runReaderT env . runWriterT . flip runStateT st

translate :: (Temp.Generator -> (Translate.Exp, Temp.Generator))
                -> Types.Ty
                -> Translator ExpTy
translate transFn typ = do
  st@SemantState{generator=gen} <- get
  let
    (resExp, gen') = transFn gen
    in do
    put st{generator=gen'}
    return ExpTy{exp=resExp, ty=typ}

translateWithFrag :: (Temp.Generator -> (Translate.Exp, Frag, Temp.Generator))
                     -> Types.Ty
                     -> Translator ExpTy
translateWithFrag transFn typ = do
  st@SemantState{generator=gen} <- get
  let
    (resExp, frag, gen') = transFn gen
    in do
    pushFrag frag
    put st{generator=gen'}
    return ExpTy{exp=resExp, ty=typ}

evalTransT :: SemantState -> SemantEnv -> Translator a
  -> Either SemantError (a, FragList)
evalTransT st env =
  runIdentity . runExceptT . flip runReaderT env . runWriterT . flip evalStateT st

nextId :: Translator Integer
nextId = do
  st@SemantState{counter'=currId} <- get
  put st{counter'=currId + 1}
  return currId

nextLabel :: Translator Temp.Label
nextLabel = do
  st@SemantState{generator=gen} <- get
  let
    (lab, gen') = Temp.newlabel gen
    in do
    put st{generator=gen'}
    return lab

newLevel :: [Frame.EscapesOrNot] -> Translator Level
newLevel escapes = do
  st@(SemantState parentLev _ _ gen) <- get
  let
    (nextLab, gen') = Temp.newlabel gen
    (gen'', lev') = newLevelFn (parentLev, nextLab, escapes) gen'
    in do
    put st{generator=gen''}
    return lev'

allocLocalT :: Bool -> Translator Access
allocLocalT escape = do
  st@(SemantState{level=lev, generator=gen}) <- get
  let
    (gen', lev', access) = allocLocalFn lev gen $ toEscape escape
    in do
    put st{level=lev', generator=gen'}
    return access

allocLocal :: Bool -> SemantState -> SemantEnv -> (Access, SemantState)
allocLocal escape st env =
  case runTransT st env $ allocLocalT escape of
    Left err -> error $ "unexpected error: " ++ show err
    Right ((access, st'), _) -> (access, st')

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
isCmp A.LeOp = True
isCmp A.GtOp = True
isCmp A.GeOp = True
isCmp _ = False


checkInt :: Types.Ty -> Maybe String-> Either String ()
checkInt Types.INT _ = Right $ ()
checkInt nonIntTy maybeCtx =
  Left $ (convertCtx maybeCtx) ++
  "expected type Ty.INT, but found " ++ show nonIntTy
  where
    convertCtx Nothing = ""
    convertCtx (Just str) = str ++ ", "

emptyExp :: Translate.Exp
emptyExp = Translate.Ex $ Tree.CONST 0

transVar (A.SimpleVar sym pos) = do
  val <- lookupT pos venv' sym
  (SemantState lev _ _ _) <- get
  case val of
    (Env.VarEntry access t) -> return ExpTy{ exp=Translate.simpleVar access lev
                                           , ty=t}
    (Env.FunEntry _ _ _ _) -> throwT pos $
                              "variable " ++ (show sym) ++
                              " has no non-function bindings."
transVar (A.FieldVar var sym pos) = do
  ExpTy{exp=recordExp, ty=varTy} <- transVar var
  case varTy of
    r@(Types.RECORD(sym2ty, _)) ->
      let
        symTyIxList =
          fmap (\((s,t),ix) -> (s,(t,ix))) $ zip sym2ty [0 :: Int ..]
      in
        case lookup sym symTyIxList  of
          Just (t, fieldNumber) ->
            translate (Translate.field recordExp fieldNumber) t
          Nothing -> throwT pos $
                     "in field expr, record type " ++
                     (show r) ++ " has no " ++ (show sym) ++
                     " field"
    t@(_) -> throwT pos $
             "in field expr, only record types have fields. type=" ++
             (show t)
transVar (A.SubscriptVar var expr pos) = do
  ExpTy{exp=varExp, ty=varTy} <- transVar var
  ExpTy{exp=indexExp, ty=expTy} <- transExp expr
  case varTy of
    Types.ARRAY(varEltTy, _) ->
      case expTy of
        Types.INT ->
          {- In the case of a constexpr access, we could optimize -}
          translate (Translate.subscript varExp indexExp) varEltTy
        nonIntTy@(_) -> throwT pos $
                        "in subscript expr, subscript type " ++
                        "is not an INT, is an " ++ (show nonIntTy)
    nonArrayTy@(_) -> throwT pos $
                      "in subscript expr, only arrays may " ++
                      "be subscripted -- attempting to subscript type=" ++
                      (show nonArrayTy)

transExp (A.VarExp var) = do
  st <- get
  env <- askEnv
  case runTransT st env $ transVar var of
    Left err -> throwErr err
    Right ((res, newState), _) -> do
      put newState
      return res
transExp A.NilExp = do
  return ExpTy{exp=Translate.nilexp, ty=Types.NIL}
transExp (A.IntExp i) = do
  return ExpTy{exp=Translate.intexp i, ty=Types.INT}
transExp (A.StringExp str) =
  translateWithFrag (Translate.string str) Types.STRING
transExp (A.CallExp funcSym argExps pos) = do
  funEntry <- lookupT pos venv' funcSym
  case funEntry of
    (Env.FunEntry funLevel label formalsTys resultTy) -> do
      paramExpTys <- mapM transExp argExps
      let paramTys = fmap ty paramExpTys in
        if (length formalsTys) /= (length paramTys)
        then
          throwT pos $ "function " ++ (show funcSym) ++
          " expects " ++ (show $ length formalsTys) ++
          " parameters but was passed " ++ (show $ length paramTys)
        else
          case filter (\(ty1, ty2, _) -> ty1 /= ty2)
               (zip3 formalsTys paramTys [0 :: Integer ..]) of
            [] -> do
              SemantState{level=thisLevel} <- get
              let
                argExpTrees = fmap exp paramExpTys
                in
                translate
                (Translate.call funLevel thisLevel label argExpTrees)
                resultTy
            ((formalTy, paramTy, ix):_) ->
              throwT pos $
              "parameter " ++ (show ix) ++ " of func " ++ (show funcSym) ++
              " requires type " ++ (show formalTy) ++
              " but was passed a value of type " ++ (show paramTy)
    (Env.VarEntry _ t) -> throwT pos $
                          "only functions are callable: found type " ++
                          (show t)
transExp (A.OpExp leftExp op rightExp pos) = do
  ExpTy{exp=expLeft, ty=tyleft} <- transExp leftExp
  ExpTy{exp=expRight, ty=tyright} <- transExp rightExp
  if isArith op then
    let maybeError = do
          _ <- checkInt tyleft (Just "in left hand operand")
          checkInt tyright (Just "in right hand operand")
    in
      case maybeError of
        Left err -> throwT pos $ "In OpExp, " ++ err
        Right _ ->
          translate (Translate.binOp expLeft expRight op) Types.INT
    else
    if isCmp op then
      case (tyleft, tyright) of
        (Types.INT, Types.INT) ->
          translate (Translate.relOp expLeft expRight op) Types.INT
        (Types.STRING, Types.STRING) ->
          translate (Translate.stringCmp expLeft expRight op) Types.INT
        (r1@(Types.RECORD _), r2@(Types.RECORD _)) ->
          if r1 == r2 then
            if isEqOrNe then
              translate (Translate.ptrCmp expLeft expRight op) Types.INT
            else
              throwT pos "records may only be compared with EQ or NE"
          else
            throwT pos "only identical record types may be compared"
        (arr1@(Types.ARRAY _), arr2@(Types.ARRAY _)) ->
          if arr1 == arr2 then
            if isEqOrNe then
              translate (Translate.ptrCmp expLeft expRight op) Types.INT
            else
              throwT pos "arrays may only be compared with EQ or NE"
          else
            throwT pos "only identical array types may be compared"
        _ -> throwT pos $
             "incomparable types " ++ (show tyleft) ++
             " and " ++ (show tyright)
    else error $ show op
  where
    isEqOrNe :: Bool
    isEqOrNe = case op of
                 A.EqOp -> True
                 A.NeqOp -> True
                 _ -> False
transExp (A.RecordExp fieldSymExpPosns typSym pos) = do
  maybeRecordTy <- lookupT pos tenv2 typSym
  case maybeRecordTy of
    recordTy@(Types.RECORD(sym2ty, _)) ->
      let
        expectedSyms = fmap fst sym2ty
        actualSyms = fmap (\(sym,_,_) -> sym) fieldSymExpPosns
      in
        if actualSyms /= expectedSyms
        then
          throwT pos $
          "incompatible field names: expected " ++
          (show expectedSyms) ++ " but record expression has " ++
          (show actualSyms)
        else do
          actualFieldExpTys <- mapM (\(_,expr,_) -> transExp expr) fieldSymExpPosns
          let
            expectedFieldTys = fmap snd sym2ty
            actualFieldTys = fmap ty actualFieldExpTys
            fieldPosns = fmap (\(_,_,fieldPos) -> fieldPos) fieldSymExpPosns
            exps = fmap exp actualFieldExpTys
            in
            case filter (\(_,expectedTy,actualTy,_) -> expectedTy /= actualTy)
                 (zip4 expectedSyms expectedFieldTys actualFieldTys fieldPosns)
            of
              [] ->
                translate (Translate.record exps) recordTy
              ((sym,expectedTy,actualTy,fieldPos):_) ->
                throwT fieldPos $
                "in record exp, field " ++ (show sym) ++
                " should have type " ++ (show expectedTy) ++
                " but has type " ++ (show actualTy)
    t@(_) ->
          throwT pos $
          "only record types may appear as the symbol in a " ++
          "record instance " ++
          "definition. Found type=" ++ (show t)
transExp (A.SeqExp expAndPosns) = do
  expTys <- mapM
    (\(expr,_) -> transExp expr)
    expAndPosns
  let
    typ = case expTys of
      [] -> Types.UNIT
      _ -> ty $ last expTys
    exps = fmap exp expTys
    transFn = case typ of
                Types.UNIT -> Translate.seqStm exps
                _ -> Translate.seqExp exps
    in
    translate transFn typ
transExp (A.AssignExp var expr pos) = do
  ExpTy{exp=lhs, ty=varTy} <- transVar var
  ExpTy{exp=rhs, ty=exprTy} <- transExp expr
  if varTy == exprTy then
    translate (Translate.assign lhs rhs) Types.UNIT
    else
    throwT pos $
    "in assignExp, variable has type " ++ (show varTy) ++
    " but assign target has type " ++ (show exprTy)
transExp (A.IfExp testExpr thenExpr maybeElseExpr pos) = do
  ExpTy{exp=testExp, ty=testTy} <- transExp testExpr
  ExpTy{exp=thenExp, ty=thenTy} <- transExp thenExpr
  let
    maybeElseExpTy = fmap transExp maybeElseExpr in
    if testTy /= Types.INT then
      throwT pos $ "in ifExp, the test expression must be integral: " ++
      "found type=" ++ (show testTy)
    else
      case maybeElseExpTy of
        Nothing ->
            if thenTy /= Types.UNIT then
               throwT pos $ "in if-then exp (without else), the if body " ++
               "must yield no value"
            else
              translate (Translate.ifThen testExp thenExp) thenTy
        Just elseExpTyM -> do
          ExpTy{exp=elseExp, ty=elseTy} <- elseExpTyM
          if thenTy /= elseTy then
            throwT pos $ "in ifExp, thenExp and elseExp must have " ++
            "the same type: found " ++ (show thenTy) ++
            " and " ++ (show elseTy) ++
            ", respectfully"
            else
            translate (Translate.ifThenElse testExp thenExp elseExp) thenTy
transExp (A.WhileExp testExp bodyExp pos) = do
  ExpTy{exp=testE, ty=testTy} <- transExp testExp
  breakTargetLab <- nextLabel
  st <- get
  put st{breakTarget=Just breakTargetLab}
  ExpTy{exp=bodyE, ty=bodyTy} <- transExp bodyExp
  put st{breakTarget=Nothing}
  if testTy /= Types.INT then
    throwT pos $ "in whileExp, the test expression must be integral: " ++
    "found type=" ++ (show testTy)
    else if bodyTy /= Types.UNIT then
    throwT pos $ "in a whileExp, the body expression must yield no value: " ++
    "found type=" ++ (show bodyTy)
    else
    translate (Translate.while testE bodyE breakTargetLab) Types.UNIT
transExp (A.BreakExp pos) = do
  SemantState{breakTarget=breakTargetMaybe} <- get
  case breakTargetMaybe of
    Nothing -> throwT pos "break expression not enclosed in a while or for"
    Just doneLabel -> return ExpTy{ exp=Translate.break doneLabel
                                  , ty=Types.UNIT}
transExp (A.ArrayExp arrayTySym sizeExp initExp pos) = do
  maybeArrayTy <- lookupT pos tenv2 arrayTySym
  case maybeArrayTy of
    arrayTy@(Types.ARRAY(arrayEltTy,_)) -> do
      ExpTy{exp=sizeExpr, ty=sizeTy} <- transExp sizeExp
      ExpTy{exp=initExpr, ty=initTy} <- transExp initExp
      if sizeTy /= Types.INT then
        throwT pos $"in ArrayExp, sizeExp must be an integer. " ++
        "Found type=" ++ (show sizeTy)
        else if initTy /= arrayEltTy then
        throwT pos $ "in ArrayExp, initExp has actual type " ++
        (show initTy) ++ ", when it must have " ++
        (show arrayEltTy)
        else
        translate (Translate.array sizeExpr initExpr) arrayTy
    t@(_) ->
          throwT pos $ "only array types may appear as the symbol in an " ++
          "array instance " ++
          "definition. Found type=" ++ (show t)
{-
In converting ForExps, the naive approach is to rewrite the AST

  for i := lo to hi
    do body

into

  let var i := lo
      var __limit := hi
  in while i <= __limit
    do (body; i := i + 1)
  end

but this potentially overflows if hi = INT_MAX.
We deal with this by instead rewriting into

  let var i := lo
      var __limit := hi
  in
  if lo <= hi
    then while 1
            do (body;
            if i < __limit then i := i + 1
                           else break);
-}
transExp (A.ForExp forVar escape loExp hiExp body pos) = do
  ExpTy{exp=_, ty=loTy} <- transExp loExp
  ExpTy{exp=_, ty=hiTy} <- transExp hiExp
  if (loTy /= Types.INT) || (hiTy /= Types.INT) then
    throwT pos "only integer expressions may appear as bounds in a ForExp"
    else
    case checkForVarNotAssigned forVar body of
      Left err -> throwErr err
      _ -> let
        forVarDec = A.VarDec{ A.name=forVar
                            , A.vardecEscape=escape
                            , A.varDecTyp=Nothing
                            , A.decInit=loExp
                            , A.decPos=pos }
        forVar_ = A.SimpleVar forVar pos
        forVarExp = A.VarExp forVar_
        limitVarName = Symbol "__limit"
        limitVarDec = A.VarDec{ A.name=limitVarName
                              , A.vardecEscape=False
                              , A.varDecTyp=Nothing
                              , A.decInit=hiExp
                              , A.decPos=pos }
        limitVarExp = A.VarExp $ A.SimpleVar limitVarName pos
        letBody = A.IfExp{ A.test=A.OpExp{ A.left=forVarExp
                                         , A.oper=A.LeOp
                                         , A.right=limitVarExp
                                         , A.pos=pos }
                         , A.then'=A.WhileExp{
                             A.test=A.IntExp 1,
                             A.body=A.SeqExp [ (body, pos)
                                             , ( A.IfExp{ A.test=A.OpExp {
                                                            A.left=forVarExp,
                                                            A.oper=A.LtOp,
                                                            A.right=limitVarExp,
                                                            A.pos=pos
                                                            }
                                                        , A.then'=A.AssignExp{
                                                            A.var=forVar_,
                                                            A.exp=A.OpExp{
                                                                A.left=forVarExp,
                                                                A.oper=A.PlusOp,
                                                                A.right=A.IntExp 1,
                                                                A.pos=pos
                                                                },
                                                              A.pos=pos
                                                            }
                                                        , A.else'=Just $ A.BreakExp pos
                                                        , A.pos=pos }
                                               , pos ) ],
                             A.pos=pos
                             }
                         , A.else'=Nothing
                         , A.pos=pos }
        ast' = A.LetExp{ A.decs=[forVarDec, limitVarDec]
                       , A.body=letBody
                       , A.pos=pos }
        in transExp ast'
transExp (A.LetExp decs bodyExp letPos) = do
  env <- askEnv
  st <- get
  case runTransT st env $ transLetDecs decs letPos of
    Left err -> throwErr err
    Right ((bodyEnv, st'), _) ->
      case
        runTransT st' bodyEnv (transExp bodyExp)
      of
        Left err -> throwErr err
        Right ((res, st''), _) -> do
          put st''
          return res

transLetDecs decls letPos = do
  env <- askEnv
  case checkDeclNamesDistinctInLet decls letPos of
    Left err -> throwErr err
    Right () ->
      foldM step env decls
  where
    step envAcc decl = do
      st <- get
      case runTransT st envAcc $ transDec decl of
        Left err -> throwErr err
        Right ((newEnv, newState), _) -> do
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
    A.FunctionDec funDecs -> fmap (\funDec -> FunDec (A.fundecName funDec) (A.funPos funDec)) funDecs
    A.VarDec name _ _ _ posn -> return $ VarDec name posn
    A.TypeDec tydecs -> fmap (\tyDec -> TyDec (A.tydecName tyDec) (A.tydecPos tyDec)) tydecs

checkForVarNotAssigned :: Symbol -> A.Exp -> Either SemantError ()
checkForVarNotAssigned forVar (A.CallExp _ exps _) =
  case sequence $ fmap (\e -> checkForVarNotAssigned forVar e) exps of
    Left err -> Left err
    Right _ -> Right ()
checkForVarNotAssigned forVar (A.OpExp leftExp _ rightExp _) = do
  checkForVarNotAssigned forVar leftExp
  checkForVarNotAssigned forVar rightExp
checkForVarNotAssigned forVar (A.RecordExp fields _ _) =
  case sequence $ fmap (\(_,e,_) -> checkForVarNotAssigned forVar e) fields of
    Left err -> Left err
    Right _ -> Right ()
checkForVarNotAssigned forVar (A.SeqExp seqElts) =
  case sequence $ fmap (\(e,_) -> checkForVarNotAssigned forVar e) seqElts of
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

transDec (A.VarDec name escape maybeTypenameAndPos initExp posn) = do
  maybeTypeAnnotation <- mapM
                         (\(typename,_) -> lookupT posn tenv2 typename)
                         maybeTypenameAndPos
  ExpTy{exp=_, ty=actualInitTy} <- transExp initExp
  env <- askEnv
  st <- get
  if actualInitTy == Types.NIL then
    case maybeTypeAnnotation of
      Just recTy@(Types.RECORD _) ->
        let
          (access, st') = allocLocal escape st env
        in do
            put st'
            return env{ venv'=Map.insert
                              name (Env.VarEntry access recTy)
                              (venv' env) }
      _ -> throwT posn $ "nil expression declarations must be " ++
           "constrained by a RECORD type"
    else
    let
      result =
        let
          (access, st') = allocLocal escape st env
        in do
          put st'
          return env { venv'=Map.insert
                            name
                            (Env.VarEntry access actualInitTy)
                            (venv' env) }
    in
      case maybeTypeAnnotation of
        Just typeAnnotation ->
          if typeAnnotation /= actualInitTy then
            throwT posn $ "mismatch in type annotation and computed " ++
            "type in varDecl: " ++
            "type annotation " ++ (show typeAnnotation) ++
            ", computed type " ++ (show actualInitTy)
          else result
        Nothing -> result
transDec (A.FunctionDec fundecs) = do
  st <- get
  env <- askEnv
  let
    resultMaybeTys =
      fmap (\fundec ->
              (join $ fmap (\(typename,_) ->
                              Map.lookup
                              typename
                              $ tenv2 env)
                $ A.result fundec))
      fundecs
    maybeFormalsTys =
      sequence $ fmap (\fundec -> sequence $
                                 fmap
                                 (computeFormalTy env)
                                 (A.params fundec)) fundecs
    in
    case maybeFormalsTys of
      Left err -> throwErr err
      Right formalsTys ->
        let
          resultTys = fmap resultTyFun resultMaybeTys
        in
          case runTransT
               st
               env
               $ extractHeaderM (venv' env) fundecs formalsTys resultTys
          of
            Left err -> throwErr err
            Right ((headerVEnv, newState), _) -> do
              put newState
              foldM
                transBody
                env{venv'=headerVEnv}
                (zip3 fundecs formalsTys resultTys)
  where
    computeFormalTy env (A.Field fieldName esc fieldTyp fieldPos) =
      case Map.lookup fieldTyp (tenv2 env) of
        Nothing -> Left SemantError{what="at parameter " ++ (show fieldName) ++
                                         " in function declaration, unbound type " ++
                                         "variable " ++ (show fieldTyp),
                                    at=fieldPos}
        Just typeTy -> Right (fieldName, typeTy, toEscape esc)
    resultTyFun maybeResultTy = case maybeResultTy of
                                  Nothing -> Types.UNIT
                                  Just typ -> typ
    transBody env (fundec,formalsTys,resultTy) = do
      st <- get
      let
        venv = venv' env
        bodyVEnv = Map.union venv $ Map.fromList $
          fmap (\(sym,typ,_) -> (sym, Env.VarEntry (formalAccess sym) typ)) formalsTys
        bodyEnv = env{venv'=bodyVEnv}
        formalAccess :: Symbol -> Access
        formalAccess sym = case
          lookup sym $ zip
            (fmap A.fieldName $ A.params fundec)
            (formalAccesses $ level st)
          of
            Just acc -> acc
            _ -> error "must not get here"
        in
        case
          runTransT st bodyEnv (transExp $ A.funBody fundec)
        of
          Left err -> throwErr err
          Right ((ExpTy{exp=_, ty=bodyTy}, state'), _) ->
            if resultTy /= Types.UNIT && resultTy /= bodyTy
            then
              throwT (A.funPos fundec) $ "computed type of function body " ++
              (show bodyTy) ++ " and annotated type " ++
              (show resultTy) ++ " do not match"
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
        env <- askEnv
        let
          step env' (CyclicSCC syms) = transCyclicDecls env' tydecs syms
          step env' (AcyclicSCC sym) = transAcyclicDecl env' tydecs sym
          in
          foldM step env stronglyConnComps

extractHeaderM :: Map.Map Symbol Env.EnvEntry
               -> [A.FunDec]
               -> [[(a, Types.Ty, c)]]
               -> [Types.Ty]
               -> Translator Env.VEnv
extractHeaderM venv fundecs formalsTys resultTys =
  foldM
  extractHeader
  venv
  (zip3 fundecs formalsTys resultTys)
  where
    extractHeader valEnv (fundec,paramTys,resultTy) =
      let
        escapes = calcEscapes fundec
      in do
        nextLev <- newLevel escapes
        nextLab <- nextLabel
        return $ Map.insert
          (A.fundecName fundec)
          Env.FunEntry{Env.level=nextLev,
                       Env.label=nextLab,
                       Env.formals=fmap (\(_,elt,_) -> elt) paramTys,
                       Env.result=resultTy}
          valEnv
    calcEscapes :: A.FunDec -> [Frame.EscapesOrNot]
    calcEscapes (A.FunDec _ params _ _ _) =
      fmap
      (\ (A.Field _ escapesOrNot _ _) -> if escapesOrNot then
                                           Frame.Escapes
                                         else
                                           Frame.NoEscape)
      params

transCyclicDecls :: SemantEnv -> [A.TyDec] -> [Symbol] -> Translator SemantEnv
transCyclicDecls env tydecs syms = do
  state <- get
  let
    tenv = tenv2 env
    headers = fmap (\sym -> (sym, Types.NAME(sym, Nothing))) syms
    bodies = fmap (\sym -> let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
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
            Right ((typeTy, newState), _) -> do
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
      Right ((translatedBodies, state'), _) -> do
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
            (sym, Types.RECORD(fmap tieFieldEntry fieldMap, recordId))
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
         $ transTy typ of
      Left err -> throwErr err
      Right ((typesTy, state'), _) -> do
        put state'
        return $ env{tenv2=Map.insert sym typesTy $ tenv2 env}

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
    typeEdges = fmap (\(sym, _, syms) -> (sym, sym, syms)) typeGraph
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
      (name, posn, fmap A.fieldTyp fields)
    calcNeighbors (A.TyDec name (A.ArrayTy(name',_)) posn) =
      (name, posn, [name'])

isCyclicSCC :: SCC vertex -> Bool
isCyclicSCC (CyclicSCC _) = True
isCyclicSCC _ = False
