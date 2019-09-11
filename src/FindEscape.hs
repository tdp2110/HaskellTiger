module FindEscape (escapeExp, findEscapes, AstDir(..), getEscape) where

import qualified Absyn as A
import Symbol

import Control.Monad (forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import Data.DList (DList, toList, singleton)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.List
import Prelude hiding (exp)


escapeExp :: A.Exp -> A.Exp

type AstPath = [AstDir]

-- a hand rolled zipper like thing. can this be generated??
data AstDir =
    CallArg Int
  | OpLeft
  | OpRight
  | RecField Int
  | SeqElt Int
  | AssignExp
  | IfTest
  | IfThen
  | IfElse
  | WhileTest
  | WhileBody
  | ForLo
  | ForHi
  | ForBody
  | LetDec Int
  | LetBody
  | ArraySize
  | ArrayInit
  | FunDec Int
  | FunParam Int
  | FunBody
  | DecInit
  deriving (Show, Eq)

data BindingKind = Fun | Var
  deriving (Show)

data EnvEntry = EnvEntry{staticDepth :: Int, path :: AstPath, kind :: BindingKind}
  deriving (Show)
type Env = Map.Map Symbol EnvEntry

data EscaperState = EscaperState{depth :: Int, env :: Env, astPath :: AstPath}
  deriving (Show)

type Escaper =  WriterT (DList (Symbol, AstPath)) (StateT EscaperState Identity)

findEscapesT :: Escaper a -> DList (Symbol, AstPath)
findEscapesT escaper =
  let
    initialState = EscaperState{depth=0, env=Map.empty, astPath=[]}
  in
    runIdentity (evalStateT (execWriterT escaper) initialState)

findEscapes :: A.Exp -> [(Symbol, AstPath)]
findEscapes exp =
  toList $ findEscapesT $ findEscapesM exp

incrStaticDepth :: Escaper ()
incrStaticDepth  = applyToStaticDepth (1+)

decrStaticDepth :: Escaper ()
decrStaticDepth = applyToStaticDepth $ \x -> x - 1

applyToStaticDepth :: (Int -> Int) -> Escaper ()
applyToStaticDepth fn = do
  state <- lift get
  lift $ put state{depth=fn $ depth state}
  return ()

pushDir :: (MonadTrans t, Monad m) =>
  EscaperState -> AstDir -> t (StateT EscaperState m) ()
pushDir state dir = lift $ put state{astPath=astPath state ++ [dir]}

pushBinding :: (MonadTrans t, Monad m,
                 Monad (t (StateT EscaperState m))) =>
  Symbol -> BindingKind -> t (StateT EscaperState m) ()
pushBinding sym bindingKind = do
  state <- lift get
  lift (put state{env=Map.insert
                      sym
                      EnvEntry{ staticDepth=depth state
                              , path=astPath state
                              , kind=bindingKind}
                      (env state)})

findEscapesM :: A.Exp -> Escaper ()
findEscapesM (A.VarExp (A.SimpleVar sym _)) = findEscapesVar sym
findEscapesM (A.VarExp (A.FieldVar var _ _)) = findEscapesVar $ findSym var
findEscapesM (A.CallExp _ args _) = forM_ args findEscapesM
findEscapesM (A.OpExp leftExp _ rightExp _) = do
  state <- lift get
  pushDir state OpLeft
  _ <- findEscapesM leftExp
  pushDir state OpRight
  _ <- findEscapesM rightExp
  lift $ put state
  return ()
findEscapesM (A.RecordExp fields _ _) = forM_ (enumerate fields) mapFun
  where
    mapFun (fieldIdx, (_, exp, _)) = do
      state <- lift get
      pushDir state (RecField fieldIdx)
      _ <- findEscapesM exp
      lift $ put state
      return ()
findEscapesM (A.SeqExp seqElts) = forM_ (enumerate seqElts) mapFun
  where
    mapFun (seqEltIdx, (exp, _)) = do
      state <- lift get
      pushDir state (SeqElt seqEltIdx)
      _ <- findEscapesM exp
      lift $ put state
      return ()
findEscapesM (A.AssignExp _ exp _) = do
  state <- lift get
  pushDir state AssignExp
  _ <- findEscapesM exp
  return ()
findEscapesM (A.IfExp testExp thenExp elseExpMaybe _) = do
  state <- lift get
  pushDir state IfTest
  _ <- findEscapesM testExp
  pushDir state IfThen
  _ <- findEscapesM thenExp
  pushDir state IfElse
  case elseExpMaybe of
    Nothing -> return ()
    Just elseExp -> do
      _ <- findEscapesM elseExp
      return ()
findEscapesM (A.WhileExp testExp bodyExp _) = do
  state <- lift get
  pushDir state WhileTest
  _ <- findEscapesM testExp
  pushDir state WhileBody
  _ <- findEscapesM bodyExp
  return ()
findEscapesM (A.ForExp forVar _ loExp hiExp bodyExp _) = do
  state <- lift get
  pushDir state ForLo
  _ <- findEscapesM loExp
  pushDir state ForHi
  _ <- findEscapesM hiExp
  pushDir state ForBody
  pushBinding forVar Var
  _ <- findEscapesM bodyExp
  return ()
findEscapesM (A.ArrayExp _ sizeExp initExp _) = do
  state <- lift get
  pushDir state ArraySize
  _ <- findEscapesM sizeExp
  pushDir state ArrayInit
  _ <- findEscapesM initExp
  return ()
findEscapesM (A.LetExp decs bodyExp _) = do
  state <- lift get
  extendEnv decs
  pushDir state LetBody
  _ <- findEscapesM bodyExp
  return ()
findEscapesM _ = do
  return ()

extendEnv :: [A.Dec] -> Escaper ()
extendEnv decs = forM_ (enumerate decs) mapFun
  where
    mapFun (decIdx, (A.FunctionDec fundecs)) = do
      state <- lift get
      pushDir state (LetDec decIdx)
      extendEnvWithFunctionDec fundecs
      lift $ put state{astPath=astPath state}
      return ()
    mapFun (decIdx, (A.VarDec sym _ _ initExp _)) = do
      state <- lift get
      pushDir state (LetDec decIdx)
      _ <- findEscapesM initExp
      lift $ put state
      pushBinding sym Var
      return ()
    mapFun (_, (A.TypeDec _)) = do return ()

extendEnvWithFunctionDec :: [A.FunDec] -> Escaper ()
extendEnvWithFunctionDec fundecs = do
  _ <- insertHeaders $ map A.fundecName fundecs
  incrStaticDepth
  _ <- processBodies $ zip (map A.params fundecs) (map A.funBody fundecs)
  decrStaticDepth
  return ()
  where
    insertHeaders :: [Symbol] -> Escaper ()
    insertHeaders syms = forM_ (enumerate syms) insertHeadersMapFun
    insertHeadersMapFun :: (Int, Symbol) -> Escaper ()
    insertHeadersMapFun (fundecIdx, sym) = do
      state <- lift get
      pushDir state (FunDec fundecIdx)
      pushBinding sym Fun
      lift $ put state
      return ()
    processBodies :: [([A.Field], A.Exp)] -> Escaper ()
    processBodies paramsAndExps = forM_
      (enumerate paramsAndExps)
      processBodiesMapFun
    processBodiesMapFun :: (Int, ([A.Field], A.Exp)) -> Escaper ()
    processBodiesMapFun (fundecIdx, (params, bodyExp)) = do
      state <- lift get
      pushDir state (FunDec fundecIdx)
      _ <- forM_ params extendByParams
      state' <- lift get
      pushDir state' FunBody
      _ <- findEscapesM bodyExp
      lift $ put state
      return ()
    extendByParams :: A.Field -> Escaper ()
    extendByParams field = do
      pushBinding (A.fieldName field) Var
      return ()

findSym :: A.Var -> Symbol
findSym (A.SimpleVar sym _) = sym
findSym (A.FieldVar var _ _) = findSym var
findSym (A.SubscriptVar var _ _) = findSym var

findEscapesVar :: Symbol -> Escaper ()
findEscapesVar sym = do
  state <- lift get
  let
    ourDepth = depth state
    theEnv = env state
    bindingMaybe = Map.lookup sym theEnv
    in
    case bindingMaybe of
      Just (EnvEntry boundDepth boundPath Var) ->
        if ourDepth > boundDepth then
          do
            tell $ singleton (sym, boundPath)
            return ()
        else
          return ()
      _ -> return ()
  return ()

escapePaths :: A.Exp -> [(Symbol, AstPath)] -> A.Exp
escapePaths exp paths =
  foldl' escapeExpPath exp paths

escapeExp exp = escapePaths exp $ findEscapes exp

escapeExpPath :: A.Exp -> (Symbol, AstPath) -> A.Exp
escapeExpPath callExp@(A.CallExp _ args _) (sym, CallArg(idx):path') =
  callExp{A.args=replaceNth idx args (escapeExpPath (args !! idx) (sym, path'))}
escapeExpPath opExp@(A.OpExp leftExp _ _ _) (sym, OpLeft:path') =
  opExp{A.left=escapeExpPath leftExp (sym, path')}
escapeExpPath opExp@(A.OpExp _ _ rightExp _) (sym, OpRight:path') =
  opExp{A.right=escapeExpPath rightExp (sym, path')}
escapeExpPath recordExp@(A.RecordExp fields _ _) (sym, RecField(idx):path') =
  let
    (sym', exp, pos) = fields !! idx
    exp' = escapeExpPath exp (sym, path')
  in
    recordExp{A.fields=replaceNth idx fields (sym', exp', pos)}
escapeExpPath (A.SeqExp seqElts) (sym, SeqElt(idx):path') =
  let
    (exp, pos) = seqElts !! idx
    exp' = escapeExpPath exp (sym, path')
  in
    A.SeqExp $ replaceNth idx seqElts (exp', pos)
escapeExpPath assignExp@(A.AssignExp _ exp _) (sym, AssignExp:path') =
  assignExp{A.exp=escapeExpPath exp (sym, path')}
escapeExpPath ifExp@(A.IfExp testExp _ _ _) (sym, IfTest:path') =
  ifExp{A.test=escapeExpPath testExp (sym, path')}
escapeExpPath ifExp@(A.IfExp _ thenExp _ _) (sym, IfThen:path') =
  ifExp{A.then'=escapeExpPath thenExp (sym, path')}
escapeExpPath ifExp@(A.IfExp _ _ (Just elseExp) _) (sym, IfElse:path') =
  ifExp{A.else'=Just $ escapeExpPath elseExp (sym, path')}
escapeExpPath whileExp@(A.WhileExp testExp _ _) (sym, WhileTest:path') =
  whileExp{A.test=escapeExpPath testExp (sym, path')}
escapeExpPath whileExp@(A.WhileExp _ bodyExp _) (sym, WhileBody:path') =
  whileExp{A.body=escapeExpPath bodyExp (sym, path')}
escapeExpPath forExp@(A.ForExp _ _ _ _ _ _) (_, []) =
  forExp{A.escape=True}
escapeExpPath forExp@(A.ForExp _ _ loExp _ _ _) (sym, ForLo:path') =
  forExp{A.lo=escapeExpPath loExp (sym, path')}
escapeExpPath forExp@(A.ForExp _ _ _ hiExp _ _) (sym, ForHi:path') =
  forExp{A.hi=escapeExpPath hiExp (sym, path')}
escapeExpPath forExp@(A.ForExp _ _ _ _ bodyExp _) (sym, ForBody:path') =
  forExp{A.body=escapeExpPath bodyExp (sym, path')}
escapeExpPath letExp@A.LetExp{A.decs=decs} (sym, []) =
  letExp{A.decs=escapeVarDec sym decs}
  where
    escapeVarDec :: Symbol -> [A.Dec] -> [A.Dec]
    escapeVarDec s ds =
      let
        Just idx = findIndex (\dec -> case dec of
                                       A.VarDec{A.name=s'} -> s == s'
                                       _ -> False)
                   ds
        varDec = ds !! idx
      in
        replaceNth idx ds $ varDec{A.vardecEscape=True}
escapeExpPath letExp@(A.LetExp decs _ _) (sym, LetDec(idx):path') =
  letExp{A.decs=replaceNth idx decs (escapeDecPath (decs !! idx) (sym, path'))}
escapeExpPath letExp@(A.LetExp _ bodyExp _) (sym, LetBody:path') =
  letExp{A.body=escapeExpPath bodyExp (sym, path')}
escapeExpPath arrayExp@(A.ArrayExp _ sizeExp _ _) (sym, ArraySize:path') =
  arrayExp{A.size=escapeExpPath sizeExp (sym, path')}
escapeExpPath arrayExp@(A.ArrayExp _ _ initExp _) (sym, ArrayInit:path') =
  arrayExp{A.init=escapeExpPath initExp (sym, path')}
escapeExpPath expr path' = error $ "shouldn't get here: expr = " ++ (show expr) ++
                          "\npath = " ++ (show path')

escapeDecPath :: A.Dec -> (Symbol, AstPath) -> A.Dec
escapeDecPath (A.FunctionDec funDecs) (sym, [FunDec(funIdx)]) =
  A.FunctionDec $ replaceNth funIdx funDecs (escapeParam (funDecs !! funIdx) sym)
  where
    escapeParam :: A.FunDec -> Symbol -> A.FunDec
    escapeParam funDec@(A.FunDec _ params _ _ _) s =
      let
        Just idx = elemIndex s $ fmap A.fieldName params
      in
        funDec{A.params=replaceNth idx params (escapeField $ params !! idx)}
    escapeField :: A.Field -> A.Field
    escapeField field@(A.Field _ _ _ _) = field{A.fieldEscape=True}
escapeDecPath (A.FunctionDec funDecs) (sym, FunDec(funIdx):FunBody:path') =
  let
    funDec = funDecs !! funIdx
    funBody = A.funBody funDec
  in
    A.FunctionDec $ replaceNth funIdx funDecs
    funDec{A.funBody=escapeExpPath funBody (sym, path')}
escapeDecPath varDec@(A.VarDec _ _ _ _ _) (_, []) = varDec{A.vardecEscape=True}
escapeDecPath varDec@(A.VarDec{A.decInit=initExp}) (sym, DecInit:path') =
  varDec{A.decInit=escapeExpPath initExp (sym, path')}
escapeDecPath dec path' = error $ "shouldn't get here: dec = " ++ (show dec) ++
                         "\npath = " ++ (show path')

getEscape :: A.Exp -> (Symbol, AstPath) -> Bool
getEscape (A.CallExp _ args _) (sym, CallArg(idx):path') =
  getEscape (args !! idx) (sym, path')
getEscape (A.OpExp leftExp _ _ _) (sym, OpLeft:path') =
  getEscape leftExp (sym, path')
getEscape (A.OpExp _ _ rightExp _) (sym, OpRight:path') =
  getEscape rightExp (sym, path')
getEscape (A.RecordExp fields _ _) (sym, RecField(idx):path') =
  let
    (_, exp, _) = fields !! idx
  in
    getEscape exp (sym, path')
getEscape (A.SeqExp seqElts) (sym, SeqElt(idx):path') =
  let
    (exp, _) = seqElts !! idx
  in
    getEscape exp (sym, path')
getEscape (A.AssignExp _ exp _) (sym, AssignExp:path') =
  getEscape exp (sym, path')
getEscape (A.IfExp testExp _ _ _) (sym, IfTest:path') =
  getEscape testExp (sym, path')
getEscape (A.IfExp _ thenExp _ _) (sym, IfThen:path') =
  getEscape thenExp (sym, path')
getEscape (A.IfExp _ _ (Just elseExp) _) (sym, IfElse:path') =
  getEscape elseExp (sym, path')
getEscape (A.WhileExp testExp _ _) (sym, WhileTest:path') =
  getEscape testExp (sym, path')
getEscape (A.WhileExp _ bodyExp _) (sym, WhileBody:path') =
  getEscape bodyExp (sym, path')
getEscape (A.ForExp _ escape _ _ _ _) (_, []) = escape
getEscape (A.ForExp _ _ loExp _ _ _) (sym, ForLo:path') =
  getEscape loExp (sym, path')
getEscape (A.ForExp _ _ _ hiExp _ _) (sym, ForHi:path') =
  getEscape hiExp (sym, path')
getEscape (A.ForExp _ _ _ _ bodyExp _) (sym, ForBody:path') =
  getEscape bodyExp (sym, path')
getEscape (A.LetExp decs _ _) (sym, LetDec(idx):path') =
  getEscapeDec (decs !! idx) (sym, path')
getEscape (A.LetExp _ bodyExp _) (sym, LetBody:path') =
  getEscape bodyExp (sym, path')
getEscape (A.LetExp{A.decs=decs}) (sym, []) =
  let
    Just (A.VarDec{A.vardecEscape=escape}) = vardecWithSym
  in
    escape
  where
    vardecWithSym = find isVarDecWithSym decs
    isVarDecWithSym :: A.Dec -> Bool
    isVarDecWithSym (A.VarDec{A.name=varname}) = varname == sym
    isVarDecWithSym _ = False
getEscape (A.ArrayExp _ sizeExp _ _) (sym, ArraySize:path') =
  getEscape sizeExp (sym, path')
getEscape (A.ArrayExp _ _ initExp _) (sym, ArrayInit:path') =
  getEscape initExp (sym, path')
getEscape exp path' = error $ "shouldn't get here. exp = " ++ (show exp) ++
                      "\npath = " ++ (show path')

getEscapeDec :: A.Dec -> (Symbol, AstPath) -> Bool
getEscapeDec (A.FunctionDec funDecs) (sym, [FunDec(funIdx)]) =
  getEscapeParam (funDecs !! funIdx)
  where
    getEscapeParam :: A.FunDec -> Bool
    getEscapeParam (A.FunDec{A.params=params}) =
      let
        Just field = find (\f -> A.fieldName f == sym) params
      in
        A.fieldEscape field
getEscapeDec (A.FunctionDec funDecs) (sym, FunDec(funIdx):FunBody:path') =
  let
    funDec = funDecs !! funIdx
    funBody = A.funBody funDec
  in
    getEscape funBody (sym, path')
getEscapeDec (A.VarDec{A.vardecEscape=escape}) (_, []) = escape
getEscapeDec (A.VarDec{A.decInit=initExp}) (sym, DecInit:path') =
  getEscape initExp (sym, path')
getEscapeDec dec path' = error $ "shouldn't get here: dec = " ++ (show dec) ++
                         "\npath = " ++ (show path')

replaceNth :: Int -> [a] -> a -> [a]
replaceNth idx xs replacement =
  case splitAt idx xs of
    (xs', _:xs'') -> xs' ++ [replacement] ++ xs''
    _ -> error $ "invalid split index: " ++ (show idx)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 :: Int ..]
