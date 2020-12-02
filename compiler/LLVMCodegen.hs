{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMCodegen where

import qualified Absyn                         as A
import qualified Symbol                        as S
import qualified Types

import qualified LLVM.AST                      as LL
import qualified LLVM.AST.Constant             as LL
import qualified LLVM.AST.Type                 as LL
import qualified LLVM.AST.IntegerPredicate     as LL

import qualified LLVM.IRBuilder.Module         as IRB
import qualified LLVM.IRBuilder.Monad          as IRB
import qualified LLVM.IRBuilder.Instruction    as IRB
import qualified LLVM.IRBuilder.Constant       as IRB

import           Data.Word
import           Data.ByteString.Short
import qualified Data.Map                      as M
import           Control.Monad                  ( when )
import           Control.Monad.State
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

data CodegenState = CodegenState { operands :: M.Map Text (LL.Operand, Types.Ty)
                                 , types :: M.Map Text Types.Ty
                                 , currentFunAndRetTy :: Maybe (A.FunDec, Types.Ty)
                                 }
        deriving (Eq, Show)

registerOperand
  :: MonadState CodegenState m => Text -> Types.Ty -> LL.Operand -> m ()
registerOperand name ty op =
  modify $ \env -> env { operands = M.insert name (op, ty) (operands env) }

type LLVM = IRB.ModuleBuilderT (State CodegenState)
type Codegen = IRB.IRBuilderT LLVM

zero :: LL.Operand
zero = IRB.int64 (0 :: Integer)

charStar :: LL.Type
charStar = LL.ptr LL.i8

nullptr :: LL.Operand
nullptr = LL.ConstantOperand $ LL.Null $ LL.ptr LL.i8

lltype :: Types.Ty -> LL.Type
lltype Types.INT  = LL.i64
lltype Types.NIL  = charStar
lltype Types.UNIT = LL.void
lltype t = error $ "unimplemented alternative " <> show t <> " in lltype"

codegenExp :: A.Exp -> Codegen (LL.Operand, Types.Ty)

codegenExp (A.VarExp (A.SimpleVar sym pos)) = do
  operandEnv <- gets operands
  case M.lookup (S.name sym) operandEnv of
    Just (op, opTy) -> do
      loadOp <- IRB.load op 8
      pure (loadOp, opTy)
    Nothing ->
      error $ "use of undefined variable " <> show sym <> " at " <> show pos

codegenExp A.NilExp = pure (nullptr, Types.NIL)

codegenExp (A.IntExp i) = pure (IRB.int64 $ toInteger i, Types.INT)

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

codegenExp (A.SeqExp expAndPosns) = do
  expsAndTys <- forM expAndPosns (\(e, _) -> codegenExp e)
  case expsAndTys of
    [] -> pure (zero, Types.UNIT)
    _  -> pure $ last expsAndTys

codegenExp (A.CallExp funcSym args _) = do
  argOps <- forM args $ \arg -> do
    (argOp, _) <- codegenExp arg -- TODO type check
    pure (argOp, [])
  operandEnv <- gets operands
  case M.lookup (S.name funcSym) operandEnv of
    Just (funcOp, _) -> do
      call <- IRB.call funcOp argOps
      pure (call, Types.INT) -- TODO not always int!
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

codegenExp e = error $ "unimplemented alternative in codegenExp: " <> show e

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
  operandEnv    <- gets operands
  let (divByZeroFn, _) = operandEnv M.! Text.pack "tiger_divByZero"
  _    <- IRB.call divByZeroFn []
  _    <- IRB.unreachable

  exit <- IRB.block `IRB.named` "exit"
  pure quotient

codegenDecl :: A.Dec -> Codegen ()
codegenDecl (A.FunctionDec [funDec]     ) = lift $ codegenFunDec funDec
codegenDecl (A.VarDec name _ _ initExp _) = do
  (initOp, initTy) <- codegenExp initExp
  addr             <- IRB.alloca (lltype initTy) Nothing 8
  IRB.store addr 8 initOp
  registerOperand (S.name name) initTy addr
  pure ()
codegenDecl _ = undefined

codegenFunDec :: A.FunDec -> LLVM ()
codegenFunDec f@A.FunDec { A.fundecName = name, A.params = params, A.result = resultTyMaybe, A.funBody = body }
  = mdo
    tenv       <- gets types
    stashedFun <- gets currentFunAndRetTy
    let retty = extractRetTy tenv
    modify $ \env -> env { currentFunAndRetTy = Just (f, retty) }
    registerOperand (S.name name) retty fun
    fun <- IRB.function (LL.Name $ toShortBS $ show name) args LL.i64 genBody
    modify $ \env -> env { currentFunAndRetTy = stashedFun }
    pure ()
 where
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

  args = toSig params

  toSig :: [A.Field] -> [(LL.Type, IRB.ParameterName)]
  toSig = fmap
    (\field ->
      (LL.i64, IRB.ParameterName $ toShortBS $ show $ A.fieldName field)
    )

  genBody :: [LL.Operand] -> Codegen ()
  genBody ops = do
    _entry <- IRB.block `IRB.named` "entry"
    forM_ (zip ops params) $ \(op, field) -> do
      addr <- IRB.alloca LL.i64 Nothing 8
      IRB.store addr 8 op
      registerOperand (S.name $ A.fieldName field) Types.INT addr -- TODO fixup param types
    (bodyOp, bodyTy) <- codegenExp body
    currentFunMaybe  <- gets currentFunAndRetTy
    case currentFunMaybe of
      Just (enclosingFunc, retty) -> do
        when (retty /= bodyTy) $ do
          error
            $  "In function "
            <> show (A.fundecName enclosingFunc)
            <> " defined at "
            <> show (A.funPos enclosingFunc)
            <> ", computed type of function body "
            <> show bodyTy
            <> " and annotated type "
            <> show retty
            <> " do not match"
        IRB.ret bodyOp
      Nothing -> error "impossible"

newtype InternalName = InternalName String
newtype ExternalName = ExternalName String

builtins :: [(InternalName, ExternalName, [LL.Type], LL.Type)]
builtins =
  [ ( InternalName "print_int"
    , ExternalName "tiger_printintln"
    , [LL.i64]
    , LL.void
    )
  , ( InternalName "tiger_divByZero"
    , ExternalName "tiger_divByZero"
    , []
    , LL.void
    )
  ]

emitBuiltin :: (InternalName, ExternalName, [LL.Type], LL.Type) -> LLVM ()
emitBuiltin (InternalName internalName, ExternalName externalName, argtys, retty)
  = do
    func <- IRB.extern (LL.mkName externalName) argtys retty
    registerOperand (Text.pack internalName) Types.UNIT func -- fixup function types

baseTEnv :: M.Map Text Types.Ty
baseTEnv = M.fromList [("string", Types.STRING), ("int", Types.INT)]

codegenLLVM :: A.Exp -> LL.Module
codegenLLVM e =
  flip
      evalState
      (CodegenState { operands           = M.empty
                    , types              = baseTEnv
                    , currentFunAndRetTy = Nothing
                    }
      )
    $ IRB.buildModuleT "llvm-test"
    $ do
        mapM_ emitBuiltin builtins
        codegenTop e
