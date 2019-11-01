{-# LANGUAGE TypeFamilies #-}

module X64Frame where

import qualified Frame
import qualified Temp
import qualified Tree

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (exp)


data X64Access = InFrame Int | InReg Int
  deriving (Show)

{-
On Linux and Mac (NOT windows) the first six integer or pointer arguments are passed in registers:
RDI, RSI, RDX, RCX, R8, R9

see https://en.wikipedia.org/wiki/X86_calling_conventions, "System V AMD64 ABI" subsection
-}
data X64 = X64 { rax :: Int
               , rbx :: Int
               , rcx :: Int
               , rdx :: Int
               , rbp :: Int
               , rsi :: Int
               , rdi :: Int
               , rsp :: Int
               , r8 :: Int
               , r9 :: Int
               , r10 :: Int
               , r11 :: Int
               , r12 :: Int
               , r13 :: Int
               , r14 :: Int
               , r15 :: Int
               , dividendRegister :: Int
               , divideDests :: [Int]
               , calleeSaves :: [Int]
               , callerSaves :: [Int]
               , callDests :: [Int]
               , tempMap :: Map Int String }
  deriving (Show)

data X64Frame = X64Frame { name :: Temp.Label
                         , formals :: [X64Access]
                         , locals :: [X64Access]
                         , x64 :: X64 }
  deriving (Show)

data Frag = PROC { body :: Tree.Stm
                 , fragFrame :: X64Frame }
          | STRING (Temp.Label, String)

maxNumRegisterParams :: Int
maxNumRegisterParams = 6

wordSize :: Int
wordSize = 8

exp :: X64Access -> Tree.Exp -> Tree.Exp
exp (InFrame k) framePtr = Tree.MEM $ Tree.BINOP (Tree.PLUS, framePtr, Tree.CONST k)
exp (InReg regNum) _ = Tree.TEMP regNum

frameExp :: X64Frame -> Tree.Exp
frameExp frame = Tree.TEMP $ Frame.fp frame

externalCall :: Temp.Label -> [Tree.Exp] -> Tree.Exp
externalCall funname params =
  Tree.CALL (Tree.NAME funname, params)

accessExp :: X64Frame -> X64Access -> Tree.Exp
accessExp frame acc = exp acc $ frameExp frame

staticLink :: Tree.Exp -> Tree.Exp
staticLink framePtr = Tree.MEM framePtr

numFormalsInReg :: X64Frame -> Int
numFormalsInReg frame =
  foldl' step (0 :: Int) (formals frame)
  where
    step :: Int -> X64Access -> Int
    step ct (InFrame _) = ct
    step ct (InReg _) = ct + 1

initX64 :: Temp.Generator -> (X64, Temp.Generator)
initX64 gen =
  let
    (raxId, gen') = Temp.newtemp gen
    (rbxId, gen'') = Temp.newtemp gen'
    (rcxId, gen3) = Temp.newtemp gen''
    (rdxId, gen4) = Temp.newtemp gen3
    (rbpId, gen5) = Temp.newtemp gen4
    (rsiId, gen6) = Temp.newtemp gen5
    (rdiId, gen7) = Temp.newtemp gen6
    (rspId, gen8) = Temp.newtemp gen7
    (r8Id, gen9) = Temp.newtemp gen8
    (r9Id, gen10) = Temp.newtemp gen9
    (r10Id, gen11) = Temp.newtemp gen10
    (r11Id, gen12) = Temp.newtemp gen11
    (r12Id, gen13) = Temp.newtemp gen12
    (r13Id, gen14) = Temp.newtemp gen13
    (r14Id, gen15) = Temp.newtemp gen14
    (r15Id, gen16) = Temp.newtemp gen15
  in
    ( X64 { rax=raxId
          , rbx=rbxId
          , rcx=rcxId
          , rdx=rdxId
          , rbp=rbpId
          , rsi=rsiId
          , rdi=rdiId
          , rsp=rspId
          , r8=r8Id
          , r9=r9Id
          , r10=r10Id
          , r11=r11Id
          , r12=r12Id
          , r13=r13Id
          , r14=r14Id
          , r15=r15Id
          , dividendRegister=raxId
          , divideDests=[raxId, rdxId]
          , calleeSaves=[rbxId, rdiId, rsiId]
          , callerSaves=[rbxId, rbpId, r12Id, r13Id, r14Id, r15Id]
          , callDests=[raxId, rdxId]
          , tempMap = Map.fromList [ (raxId, "RAX")
                                   , (rbxId, "RBX")
                                   , (rcxId, "RCX")
                                   , (rdxId, "RDX")
                                   , (rbpId, "RBP")
                                   , (rsiId, "RSI")
                                   , (rdiId, "RDI")
                                   , (rspId, "RSP")
                                   , (r8Id, "R8")
                                   , (r9Id, "R9")
                                   , (r10Id, "R10")
                                   , (r11Id, "R11")
                                   , (r12Id, "R12")
                                   , (r13Id, "R13")
                                   , (r14Id, "R14")
                                   , (r15Id, "R15")
                                   ] }
    , gen16 )

freshFrame :: Temp.Label -> X64 -> X64Frame
freshFrame frameName x64Inst =
  X64Frame{ name=frameName
          , formals=[]
          , locals=[]
          , x64=x64Inst }

instance Frame.Frame X64Frame where
  type (Access X64Frame) = X64Access
  type (Arch X64Frame) = X64
  newFrame = newFrame
  name = name
  allocLocal = allocLocal
  formals = formals
  locals = locals
  rv frame = rax $ x64 frame
  fp frame = rbp $ x64 frame

newFrame :: X64 -> Temp.Label -> Temp.Generator -> [Frame.EscapesOrNot] -> (Temp.Generator, X64Frame)
newFrame x64Inst frameName gen escapes =
  let
    initialFrame = freshFrame frameName x64Inst
    (gen', frame, _) = foldl' step (gen, initialFrame, 0) escapes
  in
    (gen', frame)
  where
    step :: (Temp.Generator, X64Frame, Int) -> Frame.EscapesOrNot -> (Temp.Generator, X64Frame, Int)
    step (gen', frame, numEscapesSeen) escapesOrNot = allocFormal gen' frame escapesOrNot numEscapesSeen
    allocFormal :: Temp.Generator -> X64Frame -> Frame.EscapesOrNot -> Int
      -> (Temp.Generator, X64Frame, Int)
    allocFormal gen' frame escapesOrNot numEscapesSeen =
        if not (Frame.escapes escapesOrNot) &&
           numFormalsInReg frame < maxNumRegisterParams then
          let
            (regNum, gen'') = Temp.newtemp gen'
          in
            (gen'', frame{formals=(formals frame) ++ [InReg regNum]}, numEscapesSeen)
        else
            (gen', frame{formals=(formals frame) ++ [InFrame numEscapesSeen]}, numEscapesSeen)

allocLocal :: Temp.Generator -> X64Frame -> Frame.EscapesOrNot
  -> (Temp.Generator, X64Frame, X64Access)
allocLocal gen frame escapesOrNot =
  if Frame.escapes escapesOrNot then
    let
      numLocals = length $ locals frame
      access = InFrame $ -1 - numLocals
    in
      (gen, frame{locals=(locals frame) ++ [access]}, access)
  else
    let
      (regLabel, gen') = Temp.newtemp gen
      access = InReg regLabel
    in
      (gen', frame{locals=(locals frame) ++ [access]}, access)

procEntryExit1 :: X64Frame -> Tree.Exp -> Tree.Exp
procEntryExit1 _ bodyExp = bodyExp
