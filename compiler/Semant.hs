{-# LANGUAGE OverloadedStrings #-}

module Semant
  ( ExpTy(..)
  , SemantError(..)
  , FragList
  , transThunked
  , transProg
  )
where

import qualified Absyn                         as A
import qualified Env
import qualified Frame
import qualified X64Frame
import qualified Translate
import qualified Types
import qualified Symbol
import           FindEscape                     ( escapeExp )
import qualified Temp

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad
import           Control.Monad.Trans.Except     ( ExceptT
                                                , throwE
                                                , runExceptT
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                , asks
                                                , runReaderT
                                                )
import           Control.Monad.Trans.State      ( StateT
                                                , get
                                                , put
                                                , runStateT
                                                )
import           Control.Monad.Trans.Writer     ( WriterT
                                                , runWriterT
                                                , tell
                                                )
import           Data.Functor.Identity
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.DList                    as DList
import           Data.Either
import           Data.List
import           Data.Graph
import           Prelude                 hiding ( exp )


data SemantError = SemantError{what :: String, at :: A.Pos} deriving (Eq)
instance Show SemantError where
  show (SemantError err pos) =
    "semantic issue at " ++ show pos ++ ": " ++ show err

data ExpTy = ExpTy {exp :: Translate.Exp, ty :: Types.Ty } deriving (Show)

transThunked
  :: A.Exp -> Either SemantError (ExpTy, FragList, Temp.Generator, X64Frame.X64)
transThunked expr =
  let tigerMain = X64Frame.mainName
      nilPos    = A.Pos { A.absChrOffset = -1, A.lineno = -1, A.colno = -1 }
      exprThunk = A.FunctionDec
        [ A.FunDec { A.fundecName = tigerMain
                   , A.params     = []
                   , A.result     = Nothing
                   , A.funBody    = expr
                   , A.funPos     = nilPos
                   }
        ]
      expr' = A.LetExp
        { A.decs = [exprThunk]
        , A.body = A.CallExp { A.func = tigerMain, A.args = [], A.pos = nilPos }
        , A.pos  = nilPos
        }
  in  transProg expr'

transProg
  :: A.Exp -> Either SemantError (ExpTy, FragList, Temp.Generator, X64Frame.X64)
transProg expr =
  let gen                = Temp.newGen
      (x64' , gen'     ) = X64Frame.initX64 gen
      (gen'', mainLevel) = newLevelFn
        x64'
        Nothing
        (outermost, Temp.Label $ Symbol.Symbol "__tiger_main", [])
        gen'
      (baseVEnv, gen''') = Env.baseVEnv x64' gen''
      startState         = SemantState { level       = mainLevel
                                       , breakTarget = Nothing
                                       , counter'    = 0
                                       , generator   = gen'''
                                       }
      env = SemantEnv { venv' = baseVEnv, tenv2 = Env.baseTEnv, x64 = x64' }
      res = runTransT startState env $ transExp $ escapeExp expr
  in  case res of
        Left err -> Left err
        Right ((expTy, stopState), frags) ->
          Right (expTy, frags, generator stopState, x64')

transVar :: A.Var -> Translator ExpTy
transExp :: A.Exp -> Translator ExpTy
transTy :: A.Ty -> Translator Types.Ty
transLetDecs :: [A.Dec] -> A.Pos -> Translator (SemantEnv, [Translate.Exp])

transDec :: A.Dec -> Translator (SemantEnv, [Translate.Exp])

type Level = Translate.X64Level
type Generator = Temp.Generator
type Access = Translate.X64Access
type Frag = X64Frame.Frag
formalAccesses :: Level -> [Access]
formalAccesses = Translate.x64TranslateFormals
outermost :: Level
outermost = Translate.X64Outermost
newLevelFn
  :: X64Frame.X64
  -> Maybe (Symbol.Symbol, A.Pos)
  -> (Level, Temp.Label, [Frame.EscapesOrNot])
  -> Temp.Generator
  -> (Temp.Generator, Level)
newLevelFn = Translate.x64NewLevel
allocLocalFn
  :: Level
  -> Temp.Generator
  -> Frame.EscapesOrNot
  -> (Temp.Generator, Level, Access)
allocLocalFn = Translate.x64AllocLocal

toEscape :: Bool -> Frame.EscapesOrNot
toEscape True = Frame.Escapes
toEscape _    = Frame.DoesNotEscape

data SemantEnv = SemantEnv { venv' :: Env.VEnv
                           , tenv2 :: Env.TEnv
                           , x64 :: X64Frame.X64 }
data SemantState = SemantState { level :: Level
                               , breakTarget :: Maybe Temp.Label
                               , counter' :: Integer
                               , generator :: Generator }

type FragList = DList.DList Frag

type Translator
  = StateT
      SemantState
      (WriterT FragList (ReaderT SemantEnv (ExceptT SemantError Identity)))

throwT :: A.Pos -> String -> Translator a
throwT posn str = throwErr SemantError { what = str, at = posn }

throwErr :: SemantError -> Translator a
throwErr = lift . lift . lift . throwE

pushFrag :: Frag -> Translator ()
pushFrag = lift . tell . DList.singleton

pushFrags :: Foldable t => t Frag -> Translator ()
pushFrags = mapM_ pushFrag

askEnv :: Translator SemantEnv
askEnv = (lift . lift) ask

lookupT
  :: A.Pos
  -> (SemantEnv -> Map.Map Symbol.Symbol a)
  -> Symbol.Symbol
  -> Translator a
lookupT posn f sym = do
  env <- (lift . lift) $ asks f
  case Map.lookup sym env of
    Nothing -> throwT posn $ "unbound variable " ++ show sym
    Just x  -> pure x

runTransT
  :: SemantState
  -> SemantEnv
  -> Translator a
  -> Either SemantError ((a, SemantState), FragList)
runTransT st env =
  runIdentity
    . runExceptT
    . flip runReaderT env
    . runWriterT
    . flip runStateT st

translate
  :: (Temp.Generator -> (Translate.Exp, Temp.Generator))
  -> Types.Ty
  -> Translator ExpTy
translate transFn typ = do
  st@SemantState { generator = gen } <- get
  let (resExp, gen') = transFn gen
  put st { generator = gen' }
  pure ExpTy { exp = resExp, ty = typ }

translateWithFrag
  :: (Temp.Generator -> (Translate.Exp, Frag, Temp.Generator))
  -> Types.Ty
  -> Translator ExpTy
translateWithFrag transFn typ = do
  st@SemantState { generator = gen } <- get
  let (resExp, frag, gen') = transFn gen
  pushFrag frag
  put st { generator = gen' }
  pure ExpTy { exp = resExp, ty = typ }

nextId :: Translator Integer
nextId = do
  st@SemantState { counter' = currId } <- get
  put st { counter' = currId + 1 }
  pure currId

nextLabel :: Translator Temp.Label
nextLabel = do
  st@SemantState { generator = gen } <- get
  let (lab, gen') = Temp.newlabel gen
  put st { generator = gen' }
  pure lab

newLevel
  :: [Frame.EscapesOrNot] -> Maybe (Symbol.Symbol, A.Pos) -> Translator Level
newLevel escapes debugInfo = do
  st@SemantState { level = parentLev, generator = gen } <- get
  env <- askEnv
  let (nextLab, gen') = Temp.newlabel gen
      (gen'', lev') =
        newLevelFn (x64 env) debugInfo (parentLev, nextLab, escapes) gen'
  put st { generator = gen'' }
  pure lev'

allocLocalT :: Bool -> Translator Access
allocLocalT escape = do
  st@SemantState { level = lev, generator = gen } <- get
  let (gen', lev', access) = allocLocalFn lev gen $ toEscape escape
  put st { level = lev', generator = gen' }
  pure access

allocLocal :: Bool -> SemantState -> SemantEnv -> (Access, SemantState)
allocLocal escape st env = case runTransT st env $ allocLocalT escape of
  Left  err                -> error $ "unexpected error: " ++ show err
  Right ((access, st'), _) -> (access, st')

transTy (A.NameTy   (sym, posn)) = lookupT posn tenv2 sym
transTy (A.RecordTy fields     ) = do
  symAndTys <- mapM mapFunc fields
  typeId    <- nextId
  pure $ Types.RECORD (symAndTys, typeId)
 where
  mapFunc (A.Field fieldName _ fieldTypSym fieldPos) = do
    typ <- lookupT fieldPos tenv2 fieldTypSym
    pure (fieldName, typ)

transTy (A.ArrayTy (arrayEltTypeSym, posn)) = do
  typ    <- lookupT posn tenv2 arrayEltTypeSym
  typeId <- nextId
  pure $ Types.ARRAY (typ, typeId)

isArith :: A.Oper -> Bool
isArith A.PlusOp   = True
isArith A.MinusOp  = True
isArith A.TimesOp  = True
isArith A.DivideOp = True
isArith A.ModOp    = True
isArith _          = False

isCmp :: A.Oper -> Bool
isCmp A.EqOp  = True
isCmp A.NeqOp = True
isCmp A.LtOp  = True
isCmp A.LeOp  = True
isCmp A.GtOp  = True
isCmp A.GeOp  = True
isCmp _       = False


checkInt :: Types.Ty -> Maybe String -> Either String ()
checkInt Types.INT _ = Right ()
checkInt nonIntTy maybeCtx =
  Left
    $  convertCtx maybeCtx
    ++ "expected type Ty.INT, but found "
    ++ show nonIntTy
 where
  convertCtx Nothing    = ""
  convertCtx (Just str) = str ++ ", "

transVar (A.SimpleVar sym pos) = do
  val                     <- lookupT pos venv' sym
  (SemantState lev _ _ _) <- get
  case val of
    (Env.VarEntry access t) ->
      pure ExpTy { exp = Translate.simpleVar access lev, ty = t }
    Env.FunEntry{} ->
      throwT pos $ "variable " ++ show sym ++ " has no non-function bindings."
transVar (A.FieldVar var sym pos) = do
  ExpTy { exp = recordExp, ty = varTy } <- transVar var
  case varTy of
    r@(Types.RECORD (sym2ty, _)) -> case fieldOffset sym2ty sym of
      Just (t, fieldNumber) ->
        translate (Translate.field recordExp fieldNumber) t
      Nothing ->
        throwT pos
          $  "in field expr, record type "
          ++ show r
          ++ " has no "
          ++ show sym
          ++ " field"
    t ->
      throwT pos
        $  "in field expr, only record types have fields. type="
        ++ show t
transVar (A.SubscriptVar var expr pos) = do
  ExpTy { exp = varExp, ty = varTy }   <- transVar var
  ExpTy { exp = indexExp, ty = expTy } <- transExp expr
  case varTy of
    Types.ARRAY (varEltTy, _) -> case expTy of
      Types.INT ->
        {- In the case of a constexpr access, we could optimize -}
        translate (Translate.subscript varExp indexExp) varEltTy
      nonIntTy ->
        throwT pos
          $  "in subscript expr, subscript type "
          ++ "is not an INT, is an "
          ++ show nonIntTy
    nonArrayTy ->
      throwT pos
        $  "in subscript expr, only arrays may "
        ++ "be subscripted -- attempting to subscript type="
        ++ show nonArrayTy

transExp (A.VarExp var) = transVar var
transExp A.NilExp = pure ExpTy { exp = Translate.nilexp, ty = Types.NIL }
transExp (A.IntExp i) = pure ExpTy { exp = Translate.intexp i, ty = Types.INT }
transExp (A.StringExp str) =
  translateWithFrag (Translate.string str) Types.STRING
transExp (A.CallExp funcSym argExps pos) = do
  funEntry <- lookupT pos venv' funcSym
  case funEntry of
    (Env.FunEntry funLevel label formalsTys resultTy) -> do
      paramExpTys <- mapM transExp argExps
      let paramTys = fmap ty paramExpTys
      if length formalsTys /= length paramTys
        then
          throwT pos
          $  "function "
          ++ show funcSym
          ++ " expects "
          ++ show (length formalsTys)
          ++ " parameters but was passed "
          ++ show (length paramTys)
        else
          case
            filter (\(ty1, ty2, _) -> ty1 /= ty2)
                   (zip3 formalsTys paramTys [0 :: Integer ..])
          of
            [] -> do
              SemantState { level = thisLevel } <- get
              let argExpTreeIRs = fmap exp paramExpTys
              translate
                (  Translate.call funLevel thisLevel label argExpTreeIRs
                $  resultTy
                /= Types.UNIT
                )
                resultTy
            ((formalTy, paramTy, ix) : _) ->
              throwT pos
                $  "parameter "
                ++ show ix
                ++ " of func "
                ++ show funcSym
                ++ " requires type "
                ++ show formalTy
                ++ " but was passed a value of type "
                ++ show paramTy
    (Env.VarEntry _ t) ->
      throwT pos $ "only functions are callable: found type " ++ show t
transExp (A.OpExp (A.IntExp i1) A.PlusOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ i1 + i2
transExp (A.OpExp (A.IntExp i1) A.TimesOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ i1 * i2
transExp (A.OpExp _ A.DivideOp (A.IntExp 0) pos) =
  throwT pos "Integer division by zero detected"
transExp (A.OpExp (A.IntExp i1) A.DivideOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ i1 `div` i2
transExp (A.OpExp _ A.ModOp (A.IntExp 0) pos) =
  throwT pos "Integer modulo by zero detected"
transExp (A.OpExp (A.IntExp i1) A.ModOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ i1 `mod` i2
transExp (A.OpExp (A.IntExp i1) A.EqOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 == i2
transExp (A.OpExp (A.IntExp i1) A.NeqOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 /= i2
transExp (A.OpExp (A.IntExp i1) A.LtOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 < i2
transExp (A.OpExp (A.IntExp i1) A.LeOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 <= i2
transExp (A.OpExp (A.IntExp i1) A.GtOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 > i2
transExp (A.OpExp (A.IntExp i1) A.GeOp (A.IntExp i2) _) =
  transExp $ A.IntExp $ fromEnum $ i1 >= i2
transExp (A.OpExp leftExp op rightExp pos) = do
  ExpTy { exp = expLeft, ty = tyleft }   <- transExp leftExp
  ExpTy { exp = expRight, ty = tyright } <- transExp rightExp
  if isArith op
    then
      let maybeError = do
            _ <- checkInt tyleft (Just "in left hand operand")
            checkInt tyright (Just "in right hand operand")
      in  case maybeError of
            Left err -> throwT pos $ "In OpExp, " ++ err
            Right _ ->
              translate (Translate.binOp expLeft expRight op) Types.INT
    else if isCmp op
      then case (tyleft, tyright) of
        (Types.INT, Types.INT) ->
          translate (Translate.relOp expLeft expRight op) Types.INT
        (Types.STRING, Types.STRING) ->
          translate (Translate.stringCmp expLeft expRight op) Types.INT
        (r1@(Types.RECORD _), r2@(Types.RECORD _)) -> if r1 == r2
          then ptrCmp expLeft expRight
          else throwT pos "only identical record types may be compared"
        (Types.RECORD _      , Types.NIL           ) -> ptrCmp expLeft expRight
        (Types.NIL           , Types.RECORD _      ) -> ptrCmp expLeft expRight
        (Types.NIL           , Types.NIL           ) -> ptrCmp expLeft expRight
        (arr1@(Types.ARRAY _), arr2@(Types.ARRAY _)) -> if arr1 == arr2
          then if isEqOrNe
            then translate (Translate.ptrCmp expLeft expRight op) Types.INT
            else throwT pos "arrays may only be compared with EQ or NE"
          else throwT pos "only identical array types may be compared"
        _ ->
          throwT pos
            $  "incomparable types "
            ++ show tyleft
            ++ " and "
            ++ show tyright
      else error $ show op
 where
  isEqOrNe :: Bool
  isEqOrNe = case op of
    A.EqOp  -> True
    A.NeqOp -> True
    _       -> False
  ptrCmp expLeft expRight = if isEqOrNe
    then translate (Translate.ptrCmp expLeft expRight op) Types.INT
    else throwT pos "records may only be compared with EQ or NE"
transExp (A.RecordExp fieldSymExpPosns typSym pos) = do
  maybeRecordTy <- lookupT pos tenv2 typSym
  case maybeRecordTy of
    recordTy@(Types.RECORD (sym2ty, _)) ->
      let expectedSyms = fmap fst sym2ty
          actualSyms   = fmap (\(sym, _, _) -> sym) fieldSymExpPosns
      in  if actualSyms /= expectedSyms
            then
              throwT pos
              $  "incompatible field names: expected "
              ++ show expectedSyms
              ++ " but record expression has "
              ++ show actualSyms
            else do
              actualFieldExpTys <- mapM (\(_, expr, _) -> transExp expr)
                                        fieldSymExpPosns
              let
                expectedFieldTys = fmap snd sym2ty
                actualFieldTys   = fmap ty actualFieldExpTys
                fieldPosns =
                  fmap (\(_, _, fieldPos) -> fieldPos) fieldSymExpPosns
                exps = fmap exp actualFieldExpTys
              case
                  filter
                    (\(_, expectedTy, actualTy, _) ->
                      not $ typesAreCompatible expectedTy actualTy
                    )
                    (zip4 expectedSyms
                          expectedFieldTys
                          actualFieldTys
                          fieldPosns
                    )
                of
                  [] -> translate (Translate.record exps) recordTy
                  ((sym, expectedTy, actualTy, fieldPos) : _) ->
                    throwT fieldPos
                      $  "in record exp, field "
                      ++ show sym
                      ++ " should have type "
                      ++ show expectedTy
                      ++ " but has type "
                      ++ show actualTy
    t ->
      throwT pos
        $  "only record types may appear as the symbol in a "
        ++ "record instance "
        ++ "definition. Found type="
        ++ show t
transExp (A.SeqExp expAndPosns) = do
  expTys <- mapM (\(expr, _) -> transExp expr) expAndPosns
  let typ = case expTys of
        [] -> Types.UNIT
        _  -> ty $ last expTys
      exps    = fmap exp expTys
      transFn = case typ of
        Types.UNIT -> Translate.seqStm exps
        _          -> Translate.seqExp exps
  translate transFn typ
transExp (A.AssignExp (A.SubscriptVar var subscriptExpr subscriptPos) expr pos)
  = do
    ExpTy { exp = varExp, ty = varTy }             <- transVar var
    ExpTy { exp = subscriptExp, ty = subscriptTy } <- transExp subscriptExpr
    ExpTy { exp = rhs, ty = exprTy }               <- transExp expr
    if subscriptTy /= Types.INT
      then
        throwT subscriptPos
        $  "in subscript expr, subscript type is not an INT, is "
        ++ show subscriptTy
      else case varTy of
        Types.ARRAY (arrayEltTy, _) ->
          if not $ typesAreCompatible arrayEltTy exprTy
            then
              throwT pos
              $  "in subscript expr, attempting to set a value of type "
              ++ show exprTy
              ++ " in an array of type "
              ++ show varTy
            else translate (Translate.setitem varExp subscriptExp rhs)
                           Types.UNIT
        _ ->
          throwT pos
            $  "in subscript expr, can only index into arrays. Got "
            ++ show varTy
transExp (A.AssignExp (A.FieldVar var sym fieldPos) expr assignPos) = do
  ExpTy { exp = recordExp, ty = varTy } <- transVar var
  ExpTy { exp = rhsExp, ty = rhsTy }    <- transExp expr
  case varTy of
    Types.RECORD (sym2ty, _) -> case fieldOffset sym2ty sym of
      Just (t, fieldNumber) -> if not $ typesAreCompatible t rhsTy
        then
          throwT assignPos
          $  "attempting to set field "
          ++ show sym
          ++ " in var "
          ++ show var
          ++ " of type "
          ++ show varTy
          ++ " to a value of type "
          ++ show rhsTy
          ++ " when type "
          ++ show t
          ++ " is needed."
        else translate (Translate.setField recordExp fieldNumber rhsExp)
                       Types.UNIT
      Nothing ->
        throwT fieldPos
          $  "in field expr, record type "
          ++ show varTy
          ++ " has no "
          ++ show sym
          ++ " field"
    _ ->
      throwT fieldPos
        $  "in a field expression, can only access fields in records. "
        ++ "Attempted to access field in variable "
        ++ show var
        ++ " of type "
        ++ show varTy
transExp (A.AssignExp var (A.IntExp i) pos) = do
  ExpTy { exp = lhs, ty = varTy } <- transVar var
  case varTy of
    Types.INT -> translate (Translate.assignConst lhs i) Types.UNIT
    _ -> throwT pos $ "Invalid operand in assign exp of type " ++ show varTy
transExp (A.AssignExp var expr pos) = do
  ExpTy { exp = lhs, ty = varTy }  <- transVar var
  ExpTy { exp = rhs, ty = exprTy } <- transExp expr
  if varTy == exprTy
    then translate (Translate.assign lhs rhs) Types.UNIT
    else
      throwT pos
      $  "in assignExp, variable has type "
      ++ show varTy
      ++ " but assign target has type "
      ++ show exprTy
transExp (A.IfExp testExpr thenExpr maybeElseExpr pos) = do
  ExpTy { exp = testExp, ty = testTy } <- transExp testExpr
  ExpTy { exp = thenExp, ty = thenTy } <- transExp thenExpr
  let maybeElseExpTy = fmap transExp maybeElseExpr
  if testTy /= Types.INT
    then
      throwT pos
      $  "in ifExp, the test expression must be integral: "
      ++ "found type="
      ++ show testTy
    else case maybeElseExpTy of
      Nothing -> if thenTy /= Types.UNIT
        then
          throwT pos
          $  "in if-then exp (without else), the if body "
          ++ "must yield no value"
        else translate (Translate.ifThen testExp thenExp) thenTy
      Just elseExpTyM -> do
        ExpTy { exp = elseExp, ty = elseTy } <- elseExpTyM
        if not $ typesAreCompatible thenTy elseTy
          then
            throwT pos
            $  "incompatible types found in ifExp: thenExp has type "
            ++ show thenTy
            ++ " but elseExp has type "
            ++ show elseTy
          else if thenTy == Types.UNIT
            then translate (Translate.ifThenElseStm testExp thenExp elseExp)
                           thenTy
            else translate (Translate.ifThenElse testExp thenExp elseExp) thenTy
transExp (A.WhileExp testExp bodyExp pos) = do
  ExpTy { exp = testE, ty = testTy } <- transExp testExp
  nextBreakTarget                    <- nextLabel
  st@SemantState { breakTarget = originalBreakTarget } <- get
  put st { breakTarget = Just nextBreakTarget }
  ExpTy { exp = bodyE, ty = bodyTy } <- transExp bodyExp
  st' <- get
  put st' { breakTarget = originalBreakTarget }
  if testTy /= Types.INT
    then
      throwT pos
      $  "in whileExp, the test expression must be integral: "
      ++ "found type="
      ++ show testTy
    else if bodyTy /= Types.UNIT
      then
        throwT pos
        $  "in a whileExp, the body expression must yield no value: "
        ++ "found type="
        ++ show bodyTy
      else translate (Translate.while testE bodyE nextBreakTarget) Types.UNIT
transExp (A.BreakExp pos) = do
  SemantState { breakTarget = breakTargetMaybe } <- get
  case breakTargetMaybe of
    Nothing -> throwT pos "break expression not enclosed in a while or for"
    Just doneLabel ->
      pure ExpTy { exp = Translate.break doneLabel, ty = Types.UNIT }
transExp (A.ArrayExp arrayTySym sizeExp initExp pos) = do
  maybeArrayTy <- lookupT pos tenv2 arrayTySym
  case maybeArrayTy of
    arrayTy@(Types.ARRAY (arrayEltTy, _)) -> do
      ExpTy { exp = sizeExpr, ty = sizeTy } <- transExp sizeExp
      ExpTy { exp = initExpr, ty = initTy } <- transExp initExp
      if sizeTy /= Types.INT
        then
          throwT pos
          $  "in ArrayExp, sizeExp must be an integer. "
          ++ "Found type="
          ++ show sizeTy
        else if initTy /= arrayEltTy
          then
            throwT pos
            $  "in ArrayExp, initExp has actual type "
            ++ show initTy
            ++ ", when it must have "
            ++ show arrayEltTy
          else translate (Translate.array sizeExpr initExpr) arrayTy
    t ->
      throwT pos
        $  "only array types may appear as the symbol in an "
        ++ "array instance "
        ++ "definition. Found type="
        ++ show t
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
  ExpTy { exp = _, ty = loTy } <- transExp loExp
  ExpTy { exp = _, ty = hiTy } <- transExp hiExp
  if (loTy /= Types.INT) || (hiTy /= Types.INT)
    then throwT pos "only integer expressions may appear as bounds in a ForExp"
    else case checkForVarNotAssigned forVar body of
      Left err -> throwErr err
      _ ->
        let
          forVarDec = A.VarDec { A.name         = forVar
                               , A.vardecEscape = escape
                               , A.varDecTyp    = Nothing
                               , A.decInit      = loExp
                               , A.decPos       = pos
                               }
          forVar_      = A.SimpleVar forVar pos
          forVarExp    = A.VarExp forVar_
          limitVarName = Symbol.Symbol "__limit"
          limitVarDec  = A.VarDec { A.name         = limitVarName
                                  , A.vardecEscape = False
                                  , A.varDecTyp    = Nothing
                                  , A.decInit      = hiExp
                                  , A.decPos       = pos
                                  }
          limitVarExp = A.VarExp $ A.SimpleVar limitVarName pos
          letBody     = A.IfExp
            { A.test  = A.OpExp { A.left  = forVarExp
                                , A.oper  = A.LeOp
                                , A.right = limitVarExp
                                , A.pos   = pos
                                }
            , A.then' = A.WhileExp
                          { A.test = A.IntExp 1
                          , A.body = A.SeqExp
                                       [ (body, pos)
                                       , ( A.IfExp
                                           { A.test  = A.OpExp
                                                         { A.left  = forVarExp
                                                         , A.oper  = A.LtOp
                                                         , A.right = limitVarExp
                                                         , A.pos   = pos
                                                         }
                                           , A.then' = A.AssignExp
                                                         { A.var = forVar_
                                                         , A.exp = A.OpExp
                                                           { A.left  = forVarExp
                                                           , A.oper  = A.PlusOp
                                                           , A.right = A.IntExp
                                                                         1
                                                           , A.pos   = pos
                                                           }
                                                         , A.pos = pos
                                                         }
                                           , A.else' = Just $ A.BreakExp pos
                                           , A.pos   = pos
                                           }
                                         , pos
                                         )
                                       ]
                          , A.pos  = pos
                          }
            , A.else' = Nothing
            , A.pos   = pos
            }
          ast' = A.LetExp { A.decs = [forVarDec, limitVarDec]
                          , A.body = letBody
                          , A.pos  = pos
                          }
        in
          transExp ast'
transExp (A.LetExp decs bodyExp letPos) = do
  env <- askEnv
  st  <- get
  case runTransT st env $ transLetDecs decs letPos of
    Left err -> throwErr err
    Right (((bodyEnv, initializers), st'), frags1) ->
      case runTransT st' bodyEnv $ transExp bodyExp of
        Left  err -> throwErr err
        Right ((ExpTy { exp = bodyTreeIR, ty = typ }, st''), frags2) -> do
          put st''
          pushFrags frags1
          pushFrags frags2
          e@ExpTy { exp = expr, ty = t } <- translate
            (Translate.letExp initializers bodyTreeIR)
            typ
          case t of
            Types.UNIT -> do
              st3@SemantState { generator = gen } <- get
              let (stm, gen') = Translate.unNx expr gen
              put st3 { generator = gen' }
              pure ExpTy { exp = Translate.Nx stm, ty = t }
            _ -> pure e

typesAreCompatible :: Types.Ty -> Types.Ty -> Bool
typesAreCompatible t1 t2 = case (t1, t2) of
  (Types.RECORD _, Types.NIL) -> True
  (Types.NIL, Types.RECORD _) -> True
  _ -> t1 == t2

fieldOffset
  :: [(Symbol.Symbol, Types.Ty)] -> Symbol.Symbol -> Maybe (Types.Ty, Int)
fieldOffset sym2ty sym =
  let symTyIxList =
          (\((s, t), ix) -> (s, (t, ix))) <$> zip sym2ty [0 :: Int ..]
  in  lookup sym symTyIxList

transLetDecs decls letPos = do
  env <- askEnv
  case checkDeclNamesDistinctInLet decls letPos of
    Left  err -> throwErr err
    Right ()  -> foldM step (env, []) decls
 where
  step (envAcc, initsAcc) decl = do
    st <- get
    case runTransT st envAcc $ transDec decl of
      Left  err -> throwErr err
      Right (((newEnv, initializers), newState), frags) -> do
        put newState
        pushFrags frags
        pure (newEnv, initsAcc ++ initializers)

checkDeclNamesDistinctInLet :: [A.Dec] -> A.Pos -> Either SemantError ()
checkDeclNamesDistinctInLet decls letPos =
  let flattenedDecls = flattenDecls decls
  in  case foldl' step (Right (Map.empty, Map.empty)) flattenedDecls of
        Left  err -> Left err
        Right _   -> Right ()
 where
  step (Left err) _ = Left err
  step (Right (funAndVarSyms, tySyms)) (TyDec name decPos) =
    case Map.lookup name tySyms of
      Nothing      -> Right (funAndVarSyms, Map.insert name decPos tySyms)
      Just decPos' -> Left SemantError
        { what = "multiple type declarations with name "
                 ++ show name
                 ++ " in letExp declarations at "
                 ++ show decPos'
                 ++ " and "
                 ++ show decPos
        , at   = letPos
        }
  step (Right (funAndVarSyms, tySyms)) decl =
    let name   = declName decl
        decPos = declPos decl
    in  case Map.lookup name funAndVarSyms of
          Nothing      -> Right (Map.insert name decPos funAndVarSyms, tySyms)
          Just decPos' -> Left SemantError
            { what = "multiple function or value declarations of symbol `"
                     ++ show name
                     ++ "` in letExp declarations at "
                     ++ show decPos'
                     ++ " and "
                     ++ show decPos
            , at   = letPos
            }

data DeclElt =
  VarDec{declName :: Symbol.Symbol, declPos :: A.Pos}
  | FunDec{declName :: Symbol.Symbol, declPos :: A.Pos}
  | TyDec{declName :: Symbol.Symbol, declPos :: A.Pos}

flattenDecls :: [A.Dec] -> [DeclElt]
flattenDecls decls = do
  decl <- decls
  case decl of
    A.FunctionDec funDecs ->
      fmap (\funDec -> FunDec (A.fundecName funDec) (A.funPos funDec)) funDecs
    A.VarDec name _ _ _ posn -> pure $ VarDec name posn
    A.TypeDec tydecs ->
      fmap (\tyDec -> TyDec (A.tydecName tyDec) (A.tydecPos tyDec)) tydecs

checkForVarNotAssigned :: Symbol.Symbol -> A.Exp -> Either SemantError ()
checkForVarNotAssigned forVar (A.CallExp _ exps _) =
  case mapM (checkForVarNotAssigned forVar) exps of
    Left  err -> Left err
    Right _   -> Right ()
checkForVarNotAssigned forVar (A.OpExp leftExp _ rightExp _) = do
  checkForVarNotAssigned forVar leftExp
  checkForVarNotAssigned forVar rightExp
checkForVarNotAssigned forVar (A.RecordExp fields _ _) =
  case mapM (\(_, e, _) -> checkForVarNotAssigned forVar e) fields of
    Left  err -> Left err
    Right _   -> Right ()
checkForVarNotAssigned forVar (A.SeqExp seqElts) =
  case mapM (\(e, _) -> checkForVarNotAssigned forVar e) seqElts of
    Left  err -> Left err
    Right _   -> Right ()
checkForVarNotAssigned forVar (A.AssignExp (A.SimpleVar var _) e pos) =
  if forVar == var
    then Left SemantError { what = "forVar assigned in forBody", at = pos }
    else checkForVarNotAssigned forVar e
checkForVarNotAssigned forVar (A.IfExp testExp thenExp maybeElseExp _) = do
  checkForVarNotAssigned forVar testExp
  checkForVarNotAssigned forVar thenExp
  case checkForVarNotAssigned forVar <$> maybeElseExp of
    Just (Left err) -> Left err
    _               -> Right ()
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
  let forVarIsRebound = all forVarIsReboundAtDec decs
      forVarIsReboundAtDec (A.VarDec varName _ _ _ _) = varName == forVar
      forVarIsReboundAtDec _                          = False
  in  if forVarIsRebound
        then Right ()
        else checkForVarNotAssigned forVar bodyExp
checkForVarNotAssigned _ _ = Right ()

transDec (A.VarDec name escape maybeTypenameAndPos initExp posn) = do
  maybeTypeAnnotation <- mapM (\(typename, _) -> lookupT posn tenv2 typename)
                              maybeTypenameAndPos
  ExpTy { exp = initExpr, ty = actualInitTy } <- transExp initExp
  env                            <- askEnv
  st@SemantState { level = lev } <- get
  if actualInitTy == Types.NIL
    then case maybeTypeAnnotation of
      Just recTy@(Types.RECORD _) ->
        let (access, st') = allocLocal escape st env
            (initTreeIR, gen') =
                Translate.initExp access lev initExpr $ generator st'
        in  do
              put st' { generator = gen' }
              -- the current level has changed! we need to update all entries in the env which use it!!
              let valenv' =
                    Map.insert name (Env.VarEntry access recTy) (venv' env)
                  valenv'' = updateLevelInEnv (level st') valenv'
              pure (env { venv' = valenv'' }, [initTreeIR])
      _ ->
        throwT posn
          $  "nil expression declarations must be "
          ++ "constrained by a RECORD type"
    else
      let
        result =
          let (access, st') = allocLocal escape st env
          in
            do
              put st'
              let (initTreeIR, gen') =
                    Translate.initExp access (level st') initExpr
                      $ generator st'
              put st' { generator = gen' }
              let
                valenv' =
                  Map.insert name (Env.VarEntry access actualInitTy) (venv' env)
                valenv'' = updateLevelInEnv (level st') valenv'
              pure (env { venv' = valenv'' }, [initTreeIR])
      in  case maybeTypeAnnotation of
            Just typeAnnotation -> if typeAnnotation /= actualInitTy
              then
                throwT posn
                $  "mismatch in type annotation and computed "
                ++ "type in varDecl: "
                ++ "type annotation "
                ++ show typeAnnotation
                ++ ", computed type "
                ++ show actualInitTy
              else result
            Nothing -> result
 where
  updateLevelInEnv
    :: Level
    -> Map.Map Symbol.Symbol Env.EnvEntry
    -> Map.Map Symbol.Symbol Env.EnvEntry
  updateLevelInEnv newLev = Map.map $ updateLevelInEntry newLev

  updateLevelInEntry :: Level -> Env.EnvEntry -> Env.EnvEntry
  updateLevelInEntry newLev entry = case entry of
    funEntry@Env.FunEntry { Env.level = oldLev } ->
      funEntry { Env.level = updateLevel oldLev newLev }
    varEntry@Env.VarEntry { Env.access = acc@Translate.X64Access { Translate.level = oldLev } }
      -> varEntry
        { Env.access = acc { Translate.level = updateLevel oldLev newLev }
        }

  updateLevel :: Level -> Level -> Level
  updateLevel oldLev newLev =
    if Translate.identifier oldLev == Translate.identifier newLev
      then newLev
      else oldLev

transDec (A.FunctionDec fundecs) = do
  st  <- get
  env <- askEnv
  let resultMaybeTys =
        ((\(typename, _) -> Map.lookup typename $ tenv2 env) <=< A.result)
          <$> fundecs
      maybeFormalsTys = mapM (mapM (computeFormalTy env) . A.params) fundecs
  case maybeFormalsTys of
    Left err -> throwErr err
    Right formalsTys ->
      let resultTys = fmap resultTyFun resultMaybeTys
      in  case
              runTransT st env
                $ extractHeaderM (venv' env) fundecs formalsTys resultTys
            of
              Left  err                             -> throwErr err
              Right ((headerVEnv, newState), frags) -> do
                put newState
                pushFrags frags
                foldM transBody
                      (env { venv' = headerVEnv }, [])
                      (zip3 fundecs formalsTys resultTys)
 where
  computeFormalTy env (A.Field fieldName esc fieldTyp fieldPos) =
    case Map.lookup fieldTyp (tenv2 env) of
      Nothing -> Left SemantError
        { what = "at parameter "
                 ++ show fieldName
                 ++ " in function declaration, unbound type "
                 ++ "variable "
                 ++ show fieldTyp
        , at   = fieldPos
        }
      Just typeTy -> Right (fieldName, typeTy, toEscape esc)
  resultTyFun = fromMaybe Types.UNIT
  transBody
    :: (SemantEnv, b)
    -> (A.FunDec, [(Symbol.Symbol, Types.Ty, c)], Types.Ty)
    -> Translator (SemantEnv, b)
  transBody (env, initializers) (A.FunDec { A.fundecName = funName, A.params = funParams, A.funBody = funBody, A.funPos = funPos }, formalsTys, resultTy)
    = do
      st <- get
      let
        venv     = venv' env
        Just Env.FunEntry { Env.level = funLevel } = Map.lookup funName venv
        paramEnv = Map.fromList $ fmap
          (\(sym, typ, _) -> (sym, Env.VarEntry (formalAccess sym) typ))
          formalsTys
        bodyVEnv = Map.union paramEnv venv
        bodyEnv  = env { venv' = bodyVEnv }
        formalAccess :: Symbol.Symbol -> Access
        formalAccess sym = case Map.lookup funName venv of
          Just Env.FunEntry { Env.level = funLev } ->
            case
                lookup sym $ zip (fmap A.fieldName funParams)
                                 (tail $ formalAccesses funLev)
              of
                Just acc -> acc
                _        -> error $ "must not get here " ++ show
                  (formalAccesses $ level st) -- drop first access, the static link
          wtf -> error $ "WTF: " ++ show wtf
      case runTransT st { level = funLevel } bodyEnv (transExp funBody) of
        Left err -> throwErr err
        Right ((ExpTy { exp = bodyExp, ty = bodyTy }, state'), frags) ->
          if resultTy /= Types.UNIT && not (typesAreCompatible resultTy bodyTy)
            then
              throwT funPos
              $  "computed type of function body "
              ++ show bodyTy
              ++ " and annotated type "
              ++ show resultTy
              ++ " do not match"
            else do
              put st { generator = generator state'
                     , counter'  = counter' state'
                     }
              pushFrags frags
              state''@SemantState { generator = gen } <- get
              let (frag, gen') =
                    Translate.functionDec (level state') bodyExp gen
              pushFrag frag
              put state'' { generator = gen' }
              pure (env, initializers)

transDec (A.TypeDec tydecs) =
  let stronglyConnComps = typeSCCs tydecs
  in  case checkForIllegalCycles tydecs stronglyConnComps of
        Left  err -> throwErr err
        Right ()  -> do
          env <- askEnv
          let step (env', initializers) (CyclicSCC syms) =
                transCyclicDecls (env', initializers) tydecs syms
              step (env', initializers) (AcyclicSCC sym) =
                transAcyclicDecl (env', initializers) tydecs sym
          foldM step (env, []) stronglyConnComps

extractHeaderM
  :: Map.Map Symbol.Symbol Env.EnvEntry
  -> [A.FunDec]
  -> [[(a, Types.Ty, c)]]
  -> [Types.Ty]
  -> Translator Env.VEnv
extractHeaderM venv fundecs formalsTys resultTys = foldM
  extractHeader
  venv
  (zip3 fundecs formalsTys resultTys)
 where
  extractHeader valEnv (fundec, paramTys, resultTy) =
    let escapes = calcEscapes fundec
    in  do
          nextLev <- newLevel escapes $ Just $ debugInfo fundec
          pure $ Map.insert
            (A.fundecName fundec)
            Env.FunEntry
              { Env.level   = nextLev
              , Env.label   = X64Frame.name $ Translate.x64Frame nextLev
              , Env.formals = fmap (\(_, elt, _) -> elt) paramTys
              , Env.result  = resultTy
              }
            valEnv
  calcEscapes :: A.FunDec -> [Frame.EscapesOrNot]
  calcEscapes (A.FunDec _ params _ _ _) = fmap
    (\(A.Field _ escapesOrNot _ _) ->
      if escapesOrNot then Frame.Escapes else Frame.DoesNotEscape
    )
    params
  debugInfo :: A.FunDec -> (Symbol.Symbol, A.Pos)
  debugInfo fundec = (A.fundecName fundec, A.funPos fundec)

transCyclicDecls
  :: (SemantEnv, [Translate.Exp])
  -> [A.TyDec]
  -> [Symbol.Symbol]
  -> Translator (SemantEnv, [Translate.Exp])
transCyclicDecls (env, initializers) tydecs syms = do
  state <- get
  let
    tenv    = tenv2 env
    headers = fmap (\sym -> (sym, Types.NAME (sym, Nothing))) syms
    bodies  = fmap
      (\sym -> let (A.TyDec _ typ _) = lookupTypeSym sym tydecs in typ)
      syms
    headerMap              = Map.fromList headers
    tenv'                  = Map.union tenv headerMap
    env'                   = env { tenv2 = tenv' }
    maybeTranslatedBodiesM = foldM
      (\acc typ -> do
        state' <- get
        case runTransT state' env' (transTy typ) of
          Left  err                     -> throwErr err
          Right ((typeTy, newState), _) -> do
            put newState
            pure $ acc ++ [typeTy]
      )
      []
      bodies
  case runTransT state env' maybeTranslatedBodiesM of
    Left  err -> throwErr err
    Right ((translatedBodies, state'), frags) -> do
      put state'
      pushFrags frags
      pure
        ( env
          { tenv2 = tieTheKnot tenv' (Map.fromList $ zip syms translatedBodies)
          }
        , initializers
        )
 where
  tieTheKnot :: Env.TEnv -> Map.Map Symbol.Symbol Types.Ty -> Env.TEnv
  tieTheKnot tenv' bodyMap =
    let newTyMap  = Map.fromList newTyList
        newTyList = [ tieEntry elt | elt <- Map.toList bodyMap ]
        tieEntry (sym, Types.RECORD (fieldMap, recordId)) =
            let tieFieldEntry
                  :: (Symbol.Symbol, Types.Ty) -> (Symbol.Symbol, Types.Ty)
                tieFieldEntry (fieldName, Types.NAME (sym', _)) =
                    (fieldName, newTyMap Map.! sym')
                tieFieldEntry (fieldName, typ) = (fieldName, typ)
            in  (sym, Types.RECORD (fmap tieFieldEntry fieldMap, recordId))
        tieEntry (sym, Types.ARRAY (Types.NAME (sym', _), arrayId)) =
            (sym, Types.ARRAY (newTyMap Map.! sym', arrayId))
        tieEntry (sym, Types.NAME (sym', _)) = (sym, newTyMap Map.! sym')
        tieEntry (sym, typ                 ) = (sym, typ)
    in  Map.union newTyMap tenv'

transAcyclicDecl
  :: (SemantEnv, [Translate.Exp])
  -> [A.TyDec]
  -> Symbol.Symbol
  -> Translator (SemantEnv, [Translate.Exp])
transAcyclicDecl (env, initializers) tydecs sym = do
  state <- get
  let (A.TyDec _ typ _) = lookupTypeSym sym tydecs
  case runTransT state env $ transTy typ of
    Left  err                        -> throwErr err
    Right ((typesTy, state'), frags) -> do
      put state'
      pushFrags frags
      pure (env { tenv2 = Map.insert sym typesTy $ tenv2 env }, initializers)

checkForIllegalCycles
  :: [A.TyDec] -> [SCC Symbol.Symbol] -> Either SemantError ()
checkForIllegalCycles tydecs stronglyConnectedComponents =
  let
    cyclicComponents        = filter isCyclicSCC stronglyConnectedComponents
    allNameCyclicComponents = filter (allAreName tydecs) cyclicComponents
  in
    case allNameCyclicComponents of
      [] -> Right ()
      (CyclicSCC syms) : _ ->
        let (A.TyDec _ _ posn) = lookupTypeSym (head syms) tydecs
        in
          Left SemantError
            { what =
              "found illegal type declaration cycle (each set of mutually "
              ++ "recursive type declarations must pass through a record or array "
              ++ "type). Cycle: "
              ++ show syms
            , at   = posn
            }
      _ -> error "shouldn't get here: we filtered on isCyclciSCC"

typeSCCs :: [A.TyDec] -> [SCC Symbol.Symbol]
typeSCCs tydecs =
  let typeGraph = calcTypeGraph tydecs
      typeEdges = fmap (\(sym, _, syms) -> (sym, sym, syms)) typeGraph
  in  reverse $ stronglyConnComp typeEdges

allAreName :: [A.TyDec] -> SCC Symbol.Symbol -> Bool
allAreName tydecs (AcyclicSCC sym ) = isNameTy tydecs sym
allAreName tydecs (CyclicSCC  syms) = all (isNameTy tydecs) syms

lookupTypeSym :: Symbol.Symbol -> [A.TyDec] -> A.TyDec
lookupTypeSym sym tydecs =
  case filter (\tydec -> A.tydecName tydec == sym) tydecs of
    []        -> error "shouldn't get here"
    tydec : _ -> tydec

isNameTy :: [A.TyDec] -> Symbol.Symbol -> Bool
isNameTy tydecs sym = case lookupTypeSym sym tydecs of
  A.TyDec _ (A.NameTy _) _ -> True
  A.TyDec{}                -> False

calcTypeGraph :: [A.TyDec] -> [(Symbol.Symbol, A.Pos, [Symbol.Symbol])]
calcTypeGraph = fmap calcNeighbors
 where
  calcNeighbors (A.TyDec name (A.NameTy (name', _)) posn) =
    (name, posn, [name'])
  calcNeighbors (A.TyDec name (A.RecordTy fields) posn) =
    (name, posn, fmap A.fieldTyp fields)
  calcNeighbors (A.TyDec name (A.ArrayTy (name', _)) posn) =
    (name, posn, [name'])

isCyclicSCC :: SCC vertex -> Bool
isCyclicSCC (CyclicSCC _) = True
isCyclicSCC _             = False
