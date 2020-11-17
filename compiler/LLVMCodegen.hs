{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVMCodegen
  ( codegen
  , SemantError
  )
where

import qualified Absyn

import           Data.Word
import           Data.String
import           Data.ByteString.Char8
import           Data.ByteString.Short
import           Data.List
import           Data.Function
import qualified Data.Map                      as Map

import           Control.Monad.State
import           Control.Applicative

import           LLVM.AST
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

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case Map.lookup nm ns of
  Nothing -> (nm, Map.insert nm 1 ns)
  Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule
  { moduleName = Data.ByteString.Short.pack $ fmap charToWord8 label
  }

type SymbolTable = [(String, Operand)]

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

data SemantError = SemantError{what :: String, at :: Absyn.Pos} deriving (Eq)
instance Show SemantError where
  show (SemantError err pos) =
    "semantic issue at " ++ show pos ++ ": " ++ show err

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

codegen :: Absyn.Exp -> Either SemantError String
codegen = undefined
