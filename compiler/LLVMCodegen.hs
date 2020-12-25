{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMCodegen where

import qualified Absyn                         as A
import qualified Symbol                        as S
import qualified Types

import qualified LLVM.AST                      as LL
import qualified LLVM.AST.Constant             as LLConstant
import qualified LLVM.AST.Type                 as LL
import qualified LLVM.AST.IntegerPredicate     as LL

import qualified LLVM.IRBuilder.Module         as IRB
import qualified LLVM.IRBuilder.Monad          as IRB
import qualified LLVM.IRBuilder.Instruction    as IRB
import qualified LLVM.IRBuilder.Constant       as IRB

import qualified Data.InfList                  as InfList
import           Data.Word
import           Data.ByteString.Short
import qualified Data.Map                      as M
import           Control.Monad                  ( when
                                                , unless
                                                , mapM
                                                , forM_
                                                )
import           Control.Monad.State
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

data FunctionType = FunctionType { paramTypes :: [Types.Ty], rettype :: Types.Ty }
        deriving (Eq, Show)

data CodegenState = CodegenState { operands :: M.Map Text (LL.Operand, Types.Ty)
                                 , functions :: M.Map Text (LL.Operand, FunctionType)
                                 , runtimeFunctions :: M.Map Text LL.Operand
                                 , types :: M.Map Text Types.Ty
                                 , strings :: M.Map Text LL.Operand
                                 , lltypes :: [(Types.Ty, LL.Type)]
                                 , currentFun :: Maybe (A.FunDec, Types.Ty, [Types.Ty])
                                 , intSupply :: InfList.InfList Integer
                                 }
        deriving (Eq, Show)

registerOperand
  :: MonadState CodegenState m => Text -> Types.Ty -> LL.Operand -> m ()
registerOperand name ty op =
  modify $ \s -> s { operands = M.insert name (op, ty) (operands s) }

registerFunction
  :: MonadState CodegenState m => Text -> FunctionType -> LL.Operand -> m ()
registerFunction name ty op =
  modify $ \s -> s { functions = M.insert name (op, ty) (functions s) }

type LLVM = IRB.ModuleBuilderT (State CodegenState)
type Codegen = IRB.IRBuilderT LLVM

nextInt :: LLVM Integer
nextInt = do
  ints <- gets intSupply
  let (res, ints') = (InfList.head ints, InfList.tail ints)
  modify $ \s -> s { intSupply = ints' }
  pure res

getRTSFunc :: String -> LLVM LL.Operand
getRTSFunc funname = do
  rtsFuncEnv <- gets runtimeFunctions
  pure $ rtsFuncEnv M.! Text.pack funname

zero :: LL.Operand
zero = IRB.int64 (0 :: Integer)

charStar :: LL.Type
charStar = LL.ptr LL.i8

nullptr :: LL.Operand
nullptr = LL.ConstantOperand $ LLConstant.Null $ LL.ptr LL.i8

lltype :: Types.Ty -> LLVM LL.Type
lltype ty = do
  lltypeMapping <- gets lltypes
  case lookup ty lltypeMapping of
    Just llt -> pure llt
    Nothing ->
      error $ "no tiger -> LLVM type mapping registered for " <> show ty

codegenVar :: A.Var -> Codegen (LL.Operand, Types.Ty)

codegenVar (A.SimpleVar sym pos) = do
  operandEnv <- gets operands
  case M.lookup (S.name sym) operandEnv of
    Just (op, opTy) -> do
      loadOp <- IRB.load op 8
      pure (loadOp, opTy)
    Nothing -> do
      funcEnv <- gets functions
      case M.lookup (S.name sym) funcEnv of
        Just _ ->
          error
            $  "variable "
            <> show sym
            <> " has no non-function bindings (at "
            <> show pos
            <> ")"
        Nothing ->
          error $ "use of undefined variable " <> show sym <> " at " <> show pos

codegenVar (A.SubscriptVar var idxExp pos) = do
  (eltPtr, eltTy) <- getArrayEltPtr var idxExp pos
  loadOp          <- IRB.load eltPtr 8
  pure (loadOp, eltTy)

codegenVar (A.FieldVar var sym pos) = do
  (fieldAddr, fieldTy) <- getFieldAddr var sym pos
  loadOp               <- IRB.load fieldAddr 8
  pure (loadOp, fieldTy)

codegenExp :: A.Exp -> Codegen (LL.Operand, Types.Ty)

codegenExp (A.VarExp var)  = codegenVar var

codegenExp A.NilExp        = pure (nullptr, Types.NIL)

codegenExp (A.IntExp    i) = pure (IRB.int64 $ toInteger i, Types.INT)

codegenExp (A.StringExp s) = do
  strs <- gets strings
  case M.lookup s strs of
    Just op -> codegenStringOp op
    Nothing -> do
      let name = LL.mkName (show (M.size strs) <> ".str")
      op <- IRB.globalStringPtr (Text.unpack s) name
      let res = LL.ConstantOperand op
      modify $ \st -> st { strings = M.insert s res strs }
      codegenStringOp res
 where
  codegenStringOp op = do
    rtsFuncEnv <- gets runtimeFunctions
    let stringCreateFn = rtsFuncEnv M.! Text.pack "tiger_allocString"
    call <- IRB.call stringCreateFn
                     [(op, []), (IRB.int64 $ toInteger (Text.length s), [])]
    pure (call, Types.STRING)

codegenExp (A.OpExp left oper right pos) = do
  (leftOperand , leftTy ) <- codegenExp left
  (rightOperand, rightTy) <- codegenExp right
  let f = case oper of
        A.PlusOp   -> IRB.add
        A.MinusOp  -> IRB.sub
        A.TimesOp  -> IRB.mul
        A.DivideOp -> codegenDivOrModulo DivOp
        A.EqOp     -> IRB.icmp LL.EQ
        A.NeqOp    -> IRB.icmp LL.NE
        A.LtOp     -> IRB.icmp LL.SLT
        A.LeOp     -> IRB.icmp LL.SLE
        A.GtOp     -> IRB.icmp LL.SGT
        A.GeOp     -> IRB.icmp LL.SGE
        A.ModOp    -> codegenDivOrModulo ModOp
  if leftTy /= Types.INT
    then
      error
      $  "invalid operand of type "
      <> show leftTy
      <> " in binary operator at "
      <> show pos
    else if rightTy /= Types.INT
      then
        error
        $  "invalid operand of type "
        <> show right
        <> " in binary operator at "
        <> show pos
      else do
        res <- f leftOperand rightOperand
        pure (res, Types.INT)

codegenExp (A.AssignExp (A.SimpleVar var varPos) rhs rhsPos) = do
  (rhsOp, rhsTy) <- codegenExp rhs
  if rhsTy == Types.UNIT
    then error $ "cannot assign a expression of type UNIT at " <> show rhsPos
    else do
      operandEnv <- gets operands
      case M.lookup (S.name var) operandEnv of
        Just (lhsOp, _) -> do
          IRB.store lhsOp 8 rhsOp
          pure (zero, Types.UNIT)
        Nothing ->
          error
            $  "use of undefined variable "
            <> show var
            <> " at "
            <> show varPos

codegenExp (A.AssignExp (A.SubscriptVar var idxExp subscriptPos) rhs rhsPos) =
  do
    (eltPtr, eltTy) <- getArrayEltPtr var idxExp subscriptPos
    (rhsOp , rhsTy) <- codegenExp rhs
    unless (typesAreCompatible rhsTy eltTy)
      $  error
      $  "In assign exp at "
      <> show rhsPos
      <> ", cannot assign a value of type "
      <> show rhsTy
      <> " into an array holding type "
      <> show eltTy
    IRB.store eltPtr 8 rhsOp
    pure (zero, Types.UNIT)

codegenExp (A.AssignExp (A.FieldVar var sym fieldPos) expr assignPos) = do
  (fieldAddr, fieldTy) <- getFieldAddr var sym fieldPos
  (rhsOp    , rhsTy  ) <- codegenExp expr
  unless (typesAreCompatible fieldTy rhsTy)
    $  error
    $  "in assign exp at "
    <> show assignPos
    <> ", attempting to set field "
    <> show sym
    <> " in var "
    <> show var
    <> " of type "
    <> show fieldTy
    <> " to a value of type "
    <> show rhsTy
  IRB.store fieldAddr 8 rhsOp
  pure (zero, Types.UNIT)

codegenExp (A.IfExp test then' (Just else') pos) = mdo
  -- %entry
  ---------
  (testOp, testTy) <- codegenExp test
  when (testTy /= Types.INT) $ do
    error
      $  "test expressions must be INT. Found "
      <> show testTy
      <> " at "
      <> show pos

  test' <- IRB.icmp LL.NE testOp zero
  IRB.condBr test' ifThen ifElse

  -- %if.then
  -----------
  ifThen               <- IRB.block `IRB.named` "if.then"
  (ifThenOp, ifThenTy) <- codegenExp then'
  IRB.br ifExit

  -- %if.else
  -----------
  ifElse               <- IRB.block `IRB.named` "if.else"
  (ifElseOp, ifElseTy) <- codegenExp else'
  when (ifThenTy /= ifElseTy) $ do
    error
      $ "In if else expressions, both ifTrue and ifFalse branches must have the same type. Found "
      <> show ifThenTy
      <> " and "
      <> show ifElseTy
      <> " at "
      <> show pos
  IRB.br ifExit

  -- %if.exit
  -----------
  ifExit <- IRB.block `IRB.named` "if.exit"
  phi    <- IRB.phi [(ifThenOp, ifThen), (ifElseOp, ifElse)]
  pure (phi, ifElseTy)

codegenExp (A.RecordExp fields (S.Symbol typeSym) pos) = do
  tenv <- gets types
  case M.lookup typeSym tenv of
    Nothing ->
      error
        $  "use of undeclared typename "
        <> show typeSym
        <> " at "
        <> show pos
    Just recordType@(Types.RECORD (symToTy, _)) ->
      let
        expectedSyms = fmap fst symToTy
        actualSyms   = fmap (\(sym, _, _) -> sym) fields
      in
        if expectedSyms /= actualSyms
          then
            error
            $  "In record exp at "
            <> show pos
            <> ", incompatible field names. Expected "
            <> show expectedSyms
            <> ", but got "
            <> show actualSyms
          else do
            let expectedFieldTys = fmap snd symToTy
            actualFieldOpTys <- mapM codegenExp
              $ fmap (\(_, expr, _) -> expr) fields
            let mismatchedTypes = filter (\(t1, (_, t2), _) -> t1 /= t2)
                  $ zip3 expectedFieldTys actualFieldOpTys [1 :: Integer ..]
            case mismatchedTypes of
              (expectedType, actualType, idx) : _ ->
                error
                  $  "In record exp at "
                  <> show pos
                  <> ", mismatched types at index "
                  <> show idx
                  <> ": expected "
                  <> show expectedType
                  <> " but found "
                  <> show actualType
              _ -> do
                allocFn      <- lift $ getRTSFunc "tiger_alloc"
                llrecordType <- lift $ lltype recordType
                let
                  recordSize32 =
                    LL.ConstantOperand $ LLConstant.sizeof $ LL.pointerReferent
                      llrecordType
                recordSize64 <- IRB.sext recordSize32 LL.i64
                dataPtr      <- IRB.call allocFn [(recordSize64, [])]
                recordPtr    <- IRB.bitcast dataPtr llrecordType
                let fieldOps = fst <$> actualFieldOpTys
                forM_
                  (zip fieldOps [0 :: Integer ..])
                  (\(fieldOp, fieldIdx) -> do
                    eltPtr <- IRB.gep recordPtr
                                      [IRB.int32 0, IRB.int32 fieldIdx]
                    IRB.store eltPtr 8 fieldOp
                  )

                pure (recordPtr, recordType)
    Just nonRecordType ->
      error
        $  "in record exp, use of non record type "
        <> show typeSym
        <> " of type "
        <> show nonRecordType
        <> " at "
        <> show pos

codegenExp (A.SeqExp expAndPosns) = do
  expsAndTys <- forM expAndPosns (\(e, _) -> codegenExp e)
  case expsAndTys of
    [] -> pure (zero, Types.UNIT)
    _  -> pure $ last expsAndTys

codegenExp (A.CallExp funcSym args pos) = do
  argOps <- forM args $ \arg -> do
    (argOp, argTy) <- codegenExp arg -- TODO type check
    pure (argOp, argTy)
  funcEnv <- gets functions
  case M.lookup (S.name funcSym) funcEnv of
    Just (funcOp, funcTy) -> do
      let argTypes = fmap snd argOps
      when (Prelude.length (paramTypes funcTy) /= Prelude.length argTypes) $ do
        error
          $  "invalid call at "
          <> show pos
          <> ": function requires "
          <> show (Prelude.length (paramTypes funcTy))
          <> " arguments, but was passed "
          <> show (Prelude.length argTypes)
      let paramTypeCheck = zip3 argTypes (paramTypes funcTy) [0 :: Int ..]
      let paramErrors = filter
            (\(actualTy, expectedTy, _) -> actualTy /= expectedTy)
            paramTypeCheck
      case paramErrors of
        ((actualTy, expectedTy, idx) : _) ->
          error
            $  "In call expression at "
            <> show pos
            <> ", parameter "
            <> show idx
            <> " has expected type "
            <> show expectedTy
            <> " but actual type "
            <> show actualTy
        _ -> do
          call <- IRB.call funcOp (fmap (\(argOp, _) -> (argOp, [])) argOps)
          pure (call, rettype funcTy)
    Nothing -> error $ "use of undeclared function " <> show funcSym

codegenExp (A.WhileExp test body pos) = mdo
  IRB.br testLab

  testLab          <- IRB.block `IRB.named` "while.test"
  (testOp, testTy) <- codegenExp test
  when (testTy /= Types.INT) $ do
    error
      $  "while test conditions must be INT. Found "
      <> show testTy
      <> " at "
      <> show pos
  test' <- IRB.icmp LL.NE testOp zero
  IRB.condBr test' loopBody loopExit

  loopBody <- IRB.block `IRB.named` "while.body"
  _        <- codegenExp body
  IRB.br testLab

  loopExit <- IRB.block `IRB.named` "while.exit"
  pure (zero, Types.UNIT)

codegenExp (A.LetExp decs body _) = do
  forM_ decs codegenDecl
  codegenExp body

codegenExp (A.ArrayExp (S.Symbol typ) sizeExp initExp pos) = do
  tenv <- gets types
  case M.lookup typ tenv of
    Nothing ->
      error
        $  "In array exp at "
        <> show pos
        <> ", use of undeclared typename "
        <> show typ
    Just t -> case t of
      Types.ARRAY (eltTy, _) -> do
        (_, initTy) <- codegenExp initExp
        if not $ typesAreCompatible initTy eltTy
          then
            error
            $  "In array exp at "
            <> show pos
            <> ", incompatible init exp of type "
            <> show initTy
            <> " in an array holding "
            <> show eltTy
          else mdo
            allocFn           <- lift $ getRTSFunc "tiger_alloc"
            llEltTy           <- lift $ lltype initTy
            arrayStructTyLLVM <- lift $ lltype t
            let arrayStructSizez32 =
                  LL.ConstantOperand $ LLConstant.sizeof $ LL.pointerReferent
                    arrayStructTyLLVM
            arrayStructSizez64 <- IRB.sext arrayStructSizez32 LL.i64
            arrayPtrI8         <- IRB.call allocFn [(arrayStructSizez64, [])]
            arrayPtr           <- IRB.bitcast arrayPtrI8 arrayStructTyLLVM

            idxAddr            <- IRB.alloca LL.i64 Nothing 8
            IRB.store idxAddr 8 zero

            szAddr           <- IRB.gep arrayPtr [IRB.int32 0, IRB.int32 0]
            (sizeOp, sizeTy) <- codegenExp sizeExp
            when (sizeTy /= Types.INT)
              $  error
              $  "In array exp, size exp must be integral. Found "
              <> show sizeTy
              <> " at "
              <> show pos
            IRB.store szAddr 8 sizeOp

            dataPtr <- IRB.gep arrayPtr [IRB.int32 0, IRB.int32 1]
            let eltSize32 = LL.ConstantOperand $ LLConstant.sizeof llEltTy
            eltSize64           <- IRB.sext eltSize32 LL.i64
            dataPtrNumBytes     <- IRB.mul sizeOp eltSize64
            arrayStorageI8      <- IRB.call allocFn [(dataPtrNumBytes, [])]
            arrayStorageEltType <- IRB.bitcast arrayStorageI8 $ LL.ptr llEltTy
            IRB.store dataPtr 8 arrayStorageEltType
            IRB.br testLab

            testLab <- IRB.block `IRB.named` "initloop.test"
            idx     <- IRB.load idxAddr 8
            test    <- IRB.icmp LL.SLT idx sizeOp
            IRB.condBr test initLoop initLoopExit

            initLoop     <- IRB.block `IRB.named` "initloop"
            (initOp, _)  <- codegenExp initExp
            idx'         <- IRB.load idxAddr 8
            eltToInitPtr <- IRB.gep arrayStorageEltType [idx']
            IRB.store eltToInitPtr 8 initOp
            idx'' <- IRB.add idx' $ IRB.int64 1
            IRB.store idxAddr 8 idx''
            IRB.br testLab

            initLoopExit <- IRB.block `IRB.named` "initloop.exit"

            pure (arrayPtr, t)
      _ ->
        error
          $  "In array exp at "
          <> show pos
          <> ", only array types may appear as symbol. Found "
          <> show t

codegenExp e = error $ "unimplemented alternative in codegenExp: " <> show e

typesAreCompatible :: Types.Ty -> Types.Ty -> Bool
typesAreCompatible t1 t2 = case (t1, t2) of
  (Types.RECORD _, Types.NIL) -> True
  (Types.NIL, Types.RECORD _) -> True
  _ -> t1 == t2

getFieldAddr :: A.Var -> S.Symbol -> A.Pos -> Codegen (LL.Operand, Types.Ty)
getFieldAddr var sym pos = do
  (varOp, varTy) <- codegenVar var
  case varTy of
    Types.RECORD (sym2ty, _) -> case fieldOffset sym2ty sym of
      Just (fieldTy, fieldNumber) -> mdo
        test <- IRB.icmp LL.NE varOp nullptr
        IRB.condBr test nonnullCase nullCase
        nonnullCase <- IRB.block `IRB.named` "nonnull"
        fieldAddr   <- IRB.gep
          varOp
          [IRB.int32 0, IRB.int32 $ fromIntegral fieldNumber]
        IRB.br exit

        nullCase          <- IRB.block `IRB.named` "null"
        nullDereferenceFn <- lift $ getRTSFunc "tiger_nullRecordDereference"
        _                 <- IRB.call nullDereferenceFn []
        _                 <- IRB.unreachable

        exit              <- IRB.block `IRB.named` "exit"
        pure (fieldAddr, fieldTy)
      Nothing ->
        error
          $  "in field expr, record type "
          <> show varTy
          <> " has no "
          <> show sym
          <> " field, at "
          <> show pos
    t ->
      error
        $  "in field expr at "
        <> show pos
        <> ", only record types have fields. type="
        <> show t

getArrayEltPtr :: A.Var -> A.Exp -> A.Pos -> Codegen (LL.Operand, Types.Ty)
getArrayEltPtr var idxExp pos = do
  (idxOp, idxTy) <- codegenExp idxExp
  when (idxTy /= Types.INT)
    $  error
    $  "In subscript expr at "
    <> show pos
    <> ", index expression must have type INT, but found "
    <> show idxTy
  (varOp, varTy) <- codegenVar var
  case varTy of
    Types.ARRAY (eltTy, _) -> mdo
      sizeAddr <- IRB.gep varOp [IRB.int32 0, IRB.int32 0]
      sizeOp   <- IRB.load sizeAddr 8
      IRB.br checkNonNegative

      checkNonNegative <- IRB.block `IRB.named` "nonnegative-check"
      isNonNegative    <- IRB.icmp LL.SLE zero idxOp
      IRB.condBr isNonNegative checkLTSize outOfRange

      checkLTSize <- IRB.block `IRB.named` "check-lt-size"
      isLTSize    <- IRB.icmp LL.SLT idxOp sizeOp
      IRB.condBr isLTSize doDereference outOfRange

      outOfRange    <- IRB.block `IRB.named` "out-of-range"
      outOfRangeFn  <- lift $ getRTSFunc "tiger_indexError"
      _             <- IRB.call outOfRangeFn [(idxOp, []), (sizeOp, [])]
      _             <- IRB.unreachable

      doDereference <- IRB.block `IRB.named` "dereference"
      dataPtrPtr    <- IRB.gep varOp [IRB.int32 0, IRB.int32 1]
      dataPtr       <- IRB.load dataPtrPtr 8
      eltPtr        <- IRB.gep dataPtr [idxOp]
      pure (eltPtr, eltTy)
    _ ->
      error
        $  "In subscript expr at "
        <> show pos
        <> ", only arrays may be subscripted. Found "
        <> show varTy

fieldOffset :: [(S.Symbol, Types.Ty)] -> S.Symbol -> Maybe (Types.Ty, Int)
fieldOffset sym2ty sym =
  let symTyIxList =
          (\((s, t), ix) -> (s, (t, ix))) <$> zip sym2ty [0 :: Int ..]
  in  lookup sym symTyIxList

emptyModule :: String -> LL.Module
emptyModule label = LL.defaultModule { LL.moduleName = toShortBS label }

codegenTop :: A.Exp -> LLVM ()
codegenTop e =
  let nilPos = A.Pos { A.absChrOffset = -1, A.lineno = -1, A.colno = -1 }
      mainFn = A.FunDec
        { A.fundecName = S.Symbol "main"
        , A.params     = []
        , A.result     = Just (S.mkSym "int", nilPos)
        , A.funBody    = A.SeqExp [(e, nilPos), (A.IntExp 0, nilPos)]
        , A.funPos     = nilPos
        }
  in  codegenFunDec mainFn

data DivOrMod = DivOp | ModOp

codegenDivOrModulo :: DivOrMod -> LL.Operand -> LL.Operand -> Codegen LL.Operand
codegenDivOrModulo divOrMod dividend divisor = mdo
  testNonzero <- IRB.icmp LL.NE divisor zero
  IRB.condBr testNonzero divisorIsNonZero divisorIsZero

  divisorIsNonZero <- IRB.block `IRB.named` "divisor_is_nonzero"
  let op = case divOrMod of
        DivOp -> IRB.sdiv
        ModOp -> IRB.srem
  quotient <- op dividend divisor
  IRB.br exit

  divisorIsZero <- IRB.block `IRB.named` "divisor_is_zero"
  divByZeroFn   <- lift $ getRTSFunc "tiger_divByZero"
  _             <- IRB.call divByZeroFn []
  _             <- IRB.unreachable

  exit          <- IRB.block `IRB.named` "exit"
  pure quotient

codegenDecl :: A.Dec -> Codegen ()

codegenDecl (A.FunctionDec [funDec]) = lift $ codegenFunDec funDec

codegenDecl (A.VarDec name _ maybeAnnotatedTy initExp pos) = do
  (initOp, initTy) <- codegenExp initExp
  eltTy            <- getEltTy initTy
  llty             <- lift $ lltype eltTy
  addr             <- IRB.alloca llty Nothing 8
  initOpCorrected  <- castNullPtrToType initOp llty initTy
  IRB.store addr 8 initOpCorrected
  registerOperand (S.name name) eltTy addr
  pure ()
 where
  castNullPtrToType initOp llty eltTy = case eltTy of
    Types.NIL -> IRB.bitcast initOp llty
    _         -> pure initOp

  getEltTy :: Types.Ty -> Codegen Types.Ty
  getEltTy initTy = case maybeAnnotatedTy of
    Nothing -> do
      pure initTy
    Just (S.Symbol annotatedTypeName, annotatedTyPos) -> do
      tenv <- gets types
      case M.lookup annotatedTypeName tenv of
        Nothing -> do
          error
            $  "use of undefined typename "
            <> show annotatedTypeName
            <> " at "
            <> show annotatedTyPos
        Just annotatedType -> if annotatedType == initTy
          then do
            pure initTy
          else
            let err = do
                  error
                    $  "in var decl at "
                    <> show pos
                    <> ", computed element type "
                    <> show initTy
                    <> " is incompatible with annotated type "
                    <> show annotatedType
            in  case initTy of
                  Types.NIL ->
                    if not (isRecordTy annotatedType)
                         && annotatedType
                         /= Types.NIL
                      then do
                        err
                      else do
                        pure annotatedType
                  _ -> do
                    err

  isRecordTy :: Types.Ty -> Bool
  isRecordTy (Types.RECORD _) = True
  isRecordTy _                = False

codegenDecl (A.TypeDec [tydec]) = lift $ codegenTypeDec tydec

codegenDecl tydec =
  error $ "unimplemented alternative in codegenDecl: " <> show tydec

codegenTypeDec :: A.TyDec -> LLVM ()
codegenTypeDec (A.TyDec (S.Symbol tydecName) ty tydecPos) = do
  tenv <- gets types
  case M.lookup tydecName tenv of
    Just _ ->
      error
        $  "redefinition of type name "
        <> show tydecName
        <> " at "
        <> show tydecPos
        <> ". Note: previous definition at ???"
    Nothing -> do
      (tigerType, llType) <- transType ty
      let tenv' = M.insert tydecName tigerType tenv
      modify $ \s -> s { types = tenv' }
      lltenv  <- gets lltypes
      llType' <- IRB.typedef (mkTypeName tigerType) (Just llType)
      modify $ \s -> s { lltypes = (tigerType, LL.ptr llType') : lltenv }
 where
  mkTypeName tigerType =
    let tydecNameStr = Text.unpack tydecName
    in  LL.mkName $ case tigerType of
          Types.RECORD (_, typeId) ->
            "struct." <> tydecNameStr <> "." <> show typeId
          Types.ARRAY (_, typeId) ->
            "array." <> tydecNameStr <> "." <> show typeId
          _ -> tydecNameStr

transType :: A.Ty -> LLVM (Types.Ty, LL.Type)
transType (A.RecordTy fields) = do
  tenv <- gets types
  let
    fieldTypesTiger = fmap
      (\A.Field { A.fieldTyp = (S.Symbol fieldTypeName), A.fieldPos = pos } ->
        case M.lookup fieldTypeName tenv of
          Nothing ->
            error
              $  "use of undefined typename "
              <> show fieldTypeName
              <> " at "
              <> show pos
          Just t -> t
      )
      fields
  typeId         <- nextInt
  fieldTypesLLVM <- mapM lltype fieldTypesTiger
  let recordTypeLLVM = LL.StructureType { LL.isPacked     = False
                                        , LL.elementTypes = fieldTypesLLVM
                                        }
  let fieldNames      = A.fieldName <$> fields
  let recordTypeTiger = Types.RECORD (zip fieldNames fieldTypesTiger, typeId)
  pure (recordTypeTiger, recordTypeLLVM)

transType (A.ArrayTy (S.Symbol sym, pos)) = do
  tenv <- gets types
  let eltTypeTiger = case M.lookup sym tenv of
        Nothing ->
          error $ "use of undefined typename " <> show sym <> " at " <> show pos
        Just t -> t
  eltTypeLLVM <- lltype eltTypeTiger
  typeId      <- nextInt
  let arrayTypeLLVM = LL.StructureType
        { LL.isPacked     = False
        , LL.elementTypes = [LL.i64, LL.ptr eltTypeLLVM]
        }
  let arrayTypeTiger = Types.ARRAY (eltTypeTiger, typeId)
  pure (arrayTypeTiger, arrayTypeLLVM)

transType t = error $ "unimplemented alternative in transType: " <> show t

codegenFunDec :: A.FunDec -> LLVM ()
codegenFunDec f@A.FunDec { A.fundecName = name, A.params = params, A.result = resultTyMaybe, A.funBody = body }
  = mdo
    tenv       <- gets types
    stashedFun <- gets currentFun
    let retTy    = extractRetTy tenv
    let paramTys = extractParamTys tenv
    modify $ \env -> env { currentFun = Just (f, retTy, paramTys) }
    registerFunction (S.name name) (FunctionType paramTys retTy) fun
    llArgs    <- getLLArgs params
    retTyLLVM <- lltype retTy
    fun       <- IRB.function (LL.Name $ toShortBS $ show name)
                              llArgs
                              retTyLLVM
                              genBody
    modify $ \env -> env { currentFun = stashedFun }
    pure ()
 where
  extractParamTys tenv = fmap
    (\field -> case M.lookup (S.name (A.fieldTyp field)) tenv of
      Just resultTy -> resultTy
      Nothing ->
        error
          $  "use of undeclared typename "
          <> show (A.fieldTyp field)
          <> " in function param declaration at "
          <> show (A.fieldPos field)
    )
    params

  extractRetTy tenv = maybe
    Types.UNIT
    (\(resultTySym, pos) -> case M.lookup (S.name resultTySym) tenv of
      Just resultTy -> resultTy
      Nothing ->
        error
          $  "use of undeclared typedef "
          <> show resultTySym
          <> " at "
          <> show pos
    )
    resultTyMaybe

  getLLArgs :: [A.Field] -> LLVM [(LL.Type, IRB.ParameterName)]
  getLLArgs fields = do
    currentFunMaybe <- gets currentFun
    case currentFunMaybe of
      Just (_, _, paramTys) -> mapM getLLArg $ zip fields paramTys
      Nothing               -> error "impossible"

  getLLArg (field, fieldTy) = do
    llty <- lltype fieldTy
    let paramName = IRB.ParameterName $ toShortBS $ show $ A.fieldName field
    pure (llty, paramName)

  genBody :: [LL.Operand] -> Codegen ()
  genBody ops = do
    _entry          <- IRB.block `IRB.named` "entry"
    currentFunMaybe <- gets currentFun
    case currentFunMaybe of
      Just (enclosingFunc, retTy, paramTys) -> do
        forM_ (zip3 ops params paramTys) $ \(op, field, fieldTy) -> do
          llfieldTy <- lift $ lltype fieldTy
          addr      <- IRB.alloca llfieldTy Nothing 8
          IRB.store addr 8 op
          registerOperand (S.name $ A.fieldName field) fieldTy addr
        (bodyOp, bodyTy) <- codegenExp body
        when (retTy /= bodyTy) $ do
          error
            $  "In function "
            <> show (A.fundecName enclosingFunc)
            <> " defined at "
            <> show (A.funPos enclosingFunc)
            <> ", computed type of function body "
            <> show bodyTy
            <> " and annotated type "
            <> show retTy
            <> " do not match"
        case retTy of
          Types.UNIT -> IRB.retVoid
          _          -> IRB.ret bodyOp
      Nothing -> error "impossible"

newtype InternalName = InternalName String
newtype ExternalName = ExternalName String

codegenLLVM :: String -> A.Exp -> LL.Module
codegenLLVM filename e =
  flip
      evalState
      (CodegenState { operands         = M.empty
                    , functions        = M.empty
                    , runtimeFunctions = M.empty
                    , types            = baseTEnv
                    , strings          = M.empty
                    , currentFun       = Nothing
                    , lltypes          = builtinLLTypes
                    , intSupply        = InfList.iterate (+ 1) 0
                    }
      )
    $ IRB.buildModuleT (toShortBS filename)
    $ do
        emitStringTypedef
        runtimeFuncs <- buildRuntimeFuncs
        mapM_ emitRuntimeFunction runtimeFuncs
        mapM_ emitBuiltin         builtins
        codegenTop e
 where
  buildRuntimeFuncs = do
    llStringType <- lltype Types.STRING
    pure
      [ ("tiger_divByZero"            , []                , LL.void)
      , ("tiger_allocString"          , [charStar, LL.i64], llStringType)
      , ("tiger_alloc"                , [LL.i64]          , charStar)
      , ("tiger_indexError"           , [LL.i64, LL.i64]  , LL.void)
      , ("tiger_nullRecordDereference", []                , LL.void)
      ]

  emitRuntimeFunction (name, argTypes, retType) = do
    func <- IRB.extern (LL.mkName name) argTypes retType
    modify $ \s -> s
      { runtimeFunctions = M.insert (Text.pack name) func (runtimeFunctions s)
      }

  emitBuiltin (InternalName internalName, ExternalName externalName, argtys, retty)
    = do
      llArgTys <- mapM lltype argtys
      llRetTy  <- lltype retty
      func     <- IRB.extern (LL.mkName externalName) llArgTys llRetTy
      let funType = FunctionType argtys retty
      registerFunction (Text.pack internalName) funType func

  baseTEnv :: M.Map Text Types.Ty
  baseTEnv = M.fromList [("string", Types.STRING), ("int", Types.INT)]

  builtinLLTypes :: [(Types.Ty, LL.Type)]
  builtinLLTypes =
    [(Types.INT, LL.i64), (Types.NIL, charStar), (Types.UNIT, LL.void)]

  emitStringTypedef :: LLVM ()
  emitStringTypedef = do
    opaqueStringType <- IRB.typedef (LL.mkName "string") Nothing
    llTypes          <- gets lltypes
    let llTypes' = (Types.STRING, LL.ptr opaqueStringType) : llTypes
    modify $ \s -> s { lltypes = llTypes' }
    pure ()

  builtins =
    [ ( InternalName "itoa"
      , ExternalName "tiger_itoa"
      , [Types.INT]
      , Types.STRING
      )
    , ( InternalName "println"
      , ExternalName "tiger_println"
      , [Types.STRING]
      , Types.UNIT
      )
    , ( InternalName "print"
      , ExternalName "tiger_print"
      , [Types.STRING]
      , Types.UNIT
      )
    , ( InternalName "size"
      , ExternalName "tiger_size"
      , [Types.STRING]
      , Types.INT
      )
    , (InternalName "getchar", ExternalName "tiger_getchar", [], Types.STRING)
    , (InternalName "getline", ExternalName "tiger_getline", [], Types.STRING)
    , (InternalName "ord", ExternalName "tiger_ord", [Types.STRING], Types.INT)
    , (InternalName "chr", ExternalName "tiger_chr", [Types.INT], Types.STRING)
    , ( InternalName "substring"
      , ExternalName "tiger_substring"
      , [Types.STRING, Types.INT, Types.INT]
      , Types.STRING
      )
    , ( InternalName "concat"
      , ExternalName "tiger_concat"
      , [Types.STRING, Types.STRING]
      , Types.STRING
      )
    , (InternalName "not" , ExternalName "tiger_not" , [Types.INT], Types.INT)
    , (InternalName "exit", ExternalName "tiger_exit", [Types.INT], Types.UNIT)
    , ( InternalName "rand"
      , ExternalName "tiger_rand"
      , [Types.INT, Types.INT]
      , Types.INT
      )
    ]
