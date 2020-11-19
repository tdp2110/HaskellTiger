{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVMTranslate where

import           Data.Word
import           Data.String
import           Data.ByteString.Char8   hiding ( reverse
                                                , count
                                                )
import           Data.ByteString.Short
import           Data.List
import           Data.Function
import qualified Data.Map                      as Map

import           Control.Monad.State
import           Control.Applicative

import           LLVM.AST
import qualified LLVM.AST.Type                 as T
import           LLVM.AST.Global
import qualified LLVM.AST                      as AST

import qualified LLVM.AST.Linkage              as L
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.Attribute            as A
import qualified LLVM.AST.CallingConvention    as CC
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

toShortBS :: String -> ShortByteString
toShortBS s = Data.ByteString.Short.pack $ fmap charToWord8 s

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case Map.lookup nm ns of
  Nothing -> (nm, Map.insert nm 1 ns)
  Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

local :: Name -> Operand
local = LocalReference T.i64

global :: Name -> C.Constant
global = C.GlobalReference T.i64

extern :: Name -> Operand
extern = ConstantOperand . C.GlobalReference T.i64

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = toShortBS label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $ GlobalDefinition $ functionDefaults
  { name        = mkName label
  , parameters  = ([ Parameter ty nm [] | (ty, nm) <- argtys ], False)
  , returnType  = retty
  , basicBlocks = body
  }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $ GlobalDefinition $ functionDefaults
  { name        = mkName label
  , linkage     = L.External
  , parameters  = ([ Parameter ty nm [] | (ty, nm) <- argtys ], False)
  , returnType  = retty
  , basicBlocks = []
  }

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new             = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks     = Map.insert (mkName qname) new bls
                   , blockCount = ix + 1
                   , names      = supply
                   }
  return (mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c    <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

type SymbolTable = [(String, Operand)]

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = fmap makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
 where
  maketerm (Just x) = x
  maketerm Nothing  = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (mkName entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i })
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

add :: Operand -> Operand -> Codegen Operand
add a b = instr $ Add { nsw          = False
                      , nuw          = False
                      , operand0     = a
                      , operand1     = b
                      , AST.metadata = []
                      }

sub :: Operand -> Operand -> Codegen Operand
sub a b = instr $ Sub { nsw          = False
                      , nuw          = False
                      , operand0     = a
                      , operand1     = b
                      , AST.metadata = []
                      }

mul :: Operand -> Operand -> Codegen Operand
mul a b = instr $ Mul { nsw          = False
                      , nuw          = False
                      , operand0     = a
                      , operand1     = b
                      , AST.metadata = []
                      }

div :: Operand -> Operand -> Codegen Operand
div a b =
  instr $ SDiv { exact = True, operand0 = a, operand1 = b, AST.metadata = [] }

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = fmap $ \x -> (x, [])

cons :: C.Constant -> Operand
cons = ConstantOperand
