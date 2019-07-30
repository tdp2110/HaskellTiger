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

data EnvEntry = EnvEntry{staticDepth :: Int, path :: AstPath}
type Env = Map.Map Symbol EnvEntry

data EscaperState = EscaperState{depth :: Int, env :: Env, astPath :: AstPath}

type Escaper =  WriterT (DList AstPath) (StateT EscaperState Identity)

findEscapesT :: Escaper a -> DList AstPath
findEscapesT escaper =
  let
    initialState = EscaperState{depth=0, env=Map.empty, astPath=[]}
  in
    runIdentity (evalStateT (execWriterT escaper) initialState)

findEscapes :: A.Exp -> [AstPath]
findEscapes exp =
  toList $ findEscapesT $ findEscapesM exp

incrStaticDepth :: Escaper ()
incrStaticDepth  = do
  state <- lift get
  lift (put state{depth=1+(depth state)})
  return ()

pushDir :: (MonadTrans t, Monad m) =>
  EscaperState -> AstDir -> t (StateT EscaperState m) ()
pushDir state dir = lift (put state{astPath=astPath state ++ [dir]})

pushBinding :: (MonadTrans t, Monad m,
                Monad (t (StateT EscaperState m))) =>
  Symbol -> t (StateT EscaperState m) ()
pushBinding sym = do
  state <- lift get
  lift (put state{env=Map.insert
                      sym
                      EnvEntry{staticDepth=depth state,
                               path=astPath state}
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
  lift (put state)
  return ()
findEscapesM (A.RecordExp fields _ _) = forM_ (enumerate fields) mapFun
  where
    mapFun (fieldIdx, (_, exp, _)) = do
      state <- lift get
      pushDir state (RecField fieldIdx)
      _ <- findEscapesM exp
      lift (put state)
      return ()
findEscapesM (A.SeqExp seqElts) = forM_ (enumerate seqElts) mapFun
  where
    mapFun (seqEltIdx, (exp, _)) = do
      state <- lift get
      pushDir state (SeqElt seqEltIdx)
      _ <- findEscapesM exp
      lift (put state)
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
  pushBinding forVar
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
      return ()
    mapFun (decIdx, (A.VarDec sym _ _ initExp _)) = do
      state <- lift get
      pushDir state (LetDec decIdx)
      _ <- findEscapesM initExp
      lift (put state)
      pushBinding sym
      return ()
    mapFun (_, (A.TypeDec _)) = do return ()

extendEnvWithFunctionDec :: [A.FunDec] -> Escaper ()
extendEnvWithFunctionDec fundecs = do
  _ <- insertHeaders (map A.fundecName fundecs)
  incrStaticDepth
  _ <- processBodies (zip (map A.params fundecs) (map A.funBody fundecs))
  return ()
  where
    insertHeaders :: [Symbol] -> Escaper ()
    insertHeaders syms = forM_ (enumerate syms) insertHeadersMapFun
    insertHeadersMapFun :: (Int, Symbol) -> Escaper ()
    insertHeadersMapFun (fundecIdx, sym) = do
      state <- lift get
      pushDir state (FunDec fundecIdx)
      pushBinding sym
      lift (put state)
      return ()
    processBodies :: [([A.Field], A.Exp)] -> Escaper ()
    processBodies paramsAndExps = forM_
      (enumerate paramsAndExps)
      processBodiesMapFun
    processBodiesMapFun :: (Int, ([A.Field], A.Exp)) -> Escaper ()
    processBodiesMapFun (fundecIdx, (params, bodyExp)) = do
      state <- lift get
      pushDir state (FunDec fundecIdx)
      _ <- forM_ (enumerate params) extendByParams
      state' <- lift get
      lift (put state'{astPath=astPath state})
      state'' <- lift get
      pushDir state'' FunBody
      _ <- findEscapesM bodyExp
      lift (put state'')
      return ()
    extendByParams :: (Int, A.Field) -> Escaper ()
    extendByParams (fieldIdx, field) = do
      state <- lift get
      pushDir state (FunParam fieldIdx)
      pushBinding (A.fieldName field)
      state' <- lift get
      lift (put state'{astPath=astPath state})
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
      Just (EnvEntry boundDepth boundPath) ->
        if ourDepth > boundDepth then
          do
            tell $ singleton boundPath
            return ()
        else
          return ()
      _ -> return ()
  return ()

escapePaths :: A.Exp -> [AstPath] -> A.Exp
escapePaths exp paths =
  foldl' escapeExpPath exp paths

escapeExp exp = escapePaths exp (findEscapes exp)

escapeExpPath :: A.Exp -> AstPath -> A.Exp
escapeExpPath callExp@(A.CallExp _ args _) (CallArg(idx):path') =
  callExp{A.args=replaceNth idx args (escapeExpPath (args !! idx) path')}
escapeExpPath opExp@(A.OpExp leftExp _ _ _) (OpLeft:path') =
  opExp{A.left=escapeExpPath leftExp path'}
escapeExpPath opExp@(A.OpExp _ _ rightExp _) (OpRight:path') =
  opExp{A.right=escapeExpPath rightExp path'}
escapeExpPath recordExp@(A.RecordExp fields _ _) (RecField(idx):path') =
  let
    (sym, exp, pos) = fields !! idx
    exp' = escapeExpPath exp path'
  in
    recordExp{A.fields=replaceNth idx fields (sym, exp', pos)}
escapeExpPath (A.SeqExp seqElts) (SeqElt(idx):path') =
  let
    (exp, pos) = seqElts !! idx
    exp' = escapeExpPath exp path'
  in
    A.SeqExp $ replaceNth idx seqElts (exp', pos)
escapeExpPath assignExp@(A.AssignExp _ exp _) (AssignExp:path') =
  assignExp{A.exp=escapeExpPath exp path'}
escapeExpPath ifExp@(A.IfExp testExp _ _ _) (IfTest:path') =
  ifExp{A.test=escapeExpPath testExp path'}
escapeExpPath ifExp@(A.IfExp _ thenExp _ _) (IfThen:path') =
  ifExp{A.then'=escapeExpPath thenExp path'}
escapeExpPath ifExp@(A.IfExp _ _ (Just elseExp) _) (IfElse:path') =
  ifExp{A.else'=Just $ escapeExpPath elseExp path'}
escapeExpPath whileExp@(A.WhileExp testExp _ _) (WhileTest:path') =
  whileExp{A.test=escapeExpPath testExp path'}
escapeExpPath whileExp@(A.WhileExp _ bodyExp _) (WhileBody:path') =
  whileExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ _ _ _) [] = forExp{A.escape=True}
escapeExpPath forExp@(A.ForExp _ _ loExp _ _ _) (ForLo:path') =
  forExp{A.lo=escapeExpPath loExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ hiExp _ _) (ForHi:path') =
  forExp{A.hi=escapeExpPath hiExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ _ bodyExp _) (ForBody:path') =
  forExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath letExp@(A.LetExp decs _ _) (LetDec(idx):path') =
  letExp{A.decs=replaceNth idx decs (escapeDecPath (decs !! idx) path')}
escapeExpPath letExp@(A.LetExp _ bodyExp _) (LetBody:path') =
  letExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath arrayExp@(A.ArrayExp _ sizeExp _ _) (ArraySize:path') =
  arrayExp{A.size=escapeExpPath sizeExp path'}
escapeExpPath arrayExp@(A.ArrayExp _ _ initExp _) (ArrayInit:path') =
  arrayExp{A.init=escapeExpPath initExp path'}
escapeExpPath _ _ = error "shouldn't get here"

escapeDecPath :: A.Dec -> AstPath -> A.Dec
escapeDecPath (A.FunctionDec funDecs) [FunDec(funIdx), FunParam(paramIdx)] =
  A.FunctionDec $ replaceNth funIdx funDecs (escapeParam (funDecs !! funIdx) paramIdx)
  where
    escapeParam :: A.FunDec -> Int -> A.FunDec
    escapeParam funDec@(A.FunDec _ params _ _ _) idx =
      funDec{A.params=replaceNth idx params (escapeField $ params !! idx)}
    escapeField :: A.Field -> A.Field
    escapeField field@(A.Field _ _ _ _) = field{A.fieldEscape=True}
escapeDecPath (A.FunctionDec funDecs) (FunDec(funIdx):FunBody:path') =
  let
    funDec = funDecs !! funIdx
    funBody = A.funBody funDec
  in
    A.FunctionDec $ replaceNth funIdx funDecs funDec{A.funBody=escapeExpPath funBody path'}
escapeDecPath varDec@(A.VarDec _ _ _ _ _) [] = varDec{A.vardecEscape=True}
escapeDecPath varDec@(A.VarDec _ _ _ initExp _) (DecInit:path') =
  varDec{A.decInit=escapeExpPath initExp path'}
escapeDecPath dec path' = error $ "shouldn't get here: dec = " ++ (show dec) ++
                         "\npath = " ++ (show path')

getEscape :: A.Exp -> AstPath -> Bool
getEscape (A.CallExp _ args _) (CallArg(idx):path') =
  getEscape (args !! idx) path'
getEscape (A.OpExp leftExp _ _ _) (OpLeft:path') =
  getEscape leftExp path'
getEscape (A.OpExp _ _ rightExp _) (OpRight:path') =
  getEscape rightExp path'
getEscape (A.RecordExp fields _ _) (RecField(idx):path') =
  let
    (_, exp, _) = fields !! idx
  in
    getEscape exp path'
getEscape (A.SeqExp seqElts) (SeqElt(idx):path') =
  let
    (exp, _) = seqElts !! idx
  in
    getEscape exp path'
getEscape (A.AssignExp _ exp _) (AssignExp:path') =
  getEscape exp path'
getEscape (A.IfExp testExp _ _ _) (IfTest:path') =
  getEscape testExp path'
getEscape (A.IfExp _ thenExp _ _) (IfThen:path') =
  getEscape thenExp path'
getEscape (A.IfExp _ _ (Just elseExp) _) (IfElse:path') =
  getEscape elseExp path'
getEscape (A.WhileExp testExp _ _) (WhileTest:path') =
  getEscape testExp path'
getEscape (A.WhileExp _ bodyExp _) (WhileBody:path') =
  getEscape bodyExp path'
getEscape (A.ForExp _ escape _ _ _ _) [] = escape
getEscape (A.ForExp _ _ loExp _ _ _) (ForLo:path') =
  getEscape loExp path'
getEscape (A.ForExp _ _ _ hiExp _ _) (ForHi:path') =
  getEscape hiExp path'
getEscape (A.ForExp _ _ _ _ bodyExp _) (ForBody:path') =
  getEscape bodyExp path'
getEscape (A.LetExp decs _ _) (LetDec(idx):path') =
  getEscapeDec (decs !! idx) path'
getEscape (A.LetExp _ bodyExp _) (LetBody:path') =
  getEscape bodyExp path'
getEscape (A.ArrayExp _ sizeExp _ _) (ArraySize:path') =
  getEscape sizeExp path'
getEscape (A.ArrayExp _ _ initExp _) (ArrayInit:path') =
  getEscape initExp path'
getEscape exp path' = error $ "shouldn't get here. exp = " ++ (show exp) ++
                      "\npath = " ++ (show path')

getEscapeDec :: A.Dec -> AstPath -> Bool
getEscapeDec (A.FunctionDec funDecs) [FunDec(funIdx), FunParam(paramIdx)] =
  getEscapeParam (funDecs !! funIdx) paramIdx
  where
    getEscapeParam :: A.FunDec -> Int -> Bool
    getEscapeParam (A.FunDec _ params _ _ _) idx =
      getEscapeField $ params !! idx
    getEscapeField :: A.Field -> Bool
    getEscapeField (A.Field _ escape _ _) = escape
getEscapeDec (A.FunctionDec funDecs) (FunDec(funIdx):FunBody:path') =
  let
    funDec = funDecs !! funIdx
    funBody = A.funBody funDec
  in
    getEscape funBody path'
getEscapeDec (A.VarDec _ escape _ _ _) [] = escape
getEscapeDec (A.VarDec _ _ _ initExp _) (DecInit:path') =
  getEscape initExp path'
getEscapeDec dec path' = error $ "shouldn't get here: dec = " ++ (show dec) ++
                         "\npath = " ++ (show path')

replaceNth :: Int -> [a] -> a -> [a]
replaceNth idx xs replacement =
  case splitAt idx xs of
    (xs', _:xs'') -> xs' ++ [replacement] ++ xs''
    _ -> error $ "invalid split index: " ++ (show idx)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 :: Int ..]
