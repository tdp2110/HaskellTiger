{-# LANGUAGE TypeFamilies #-}

module X64Frame where

import qualified Frame
import qualified Temp
import qualified Tree

import Data.List

import Prelude hiding (exp)


data X64Access = InFrame Int | InReg Int
  deriving (Show)

{-
The first six integer or pointer arguments are passed in registers:
RDI, RSI, RDX, RCX, R8, R9
-}
data X64Frame = X64Frame { name :: Temp.Label
                         , formals :: [X64Access]
                         , locals :: [X64Access]
                         , fp :: Int }
  deriving (Show)

maxNumRegisterParams :: Int
maxNumRegisterParams = 6

wordSize :: Int
wordSize = 8

exp :: X64Access -> Tree.Exp -> Tree.Exp
exp (InFrame k) framePtr = Tree.MEM $ Tree.BINOP (Tree.PLUS, framePtr, Tree.CONST k)
exp (InReg regNum) _ = Tree.TEMP regNum

frameExp :: X64Frame -> Tree.Exp
frameExp X64Frame{fp=framePtr} = Tree.TEMP framePtr

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

freshFrame :: Temp.Label -> Temp.Generator -> (X64Frame, Temp.Generator)
freshFrame frameName gen =
  let
    (fpId, gen') = Temp.newtemp gen
  in
    (X64Frame{ name=frameName
             , formals=[]
             , locals=[]
             , fp=fpId }, gen')

instance Frame.Frame X64Frame where
  type (Access X64Frame) = X64Access
  newFrame = newFrame
  name = name
  allocLocal = allocLocal
  formals = formals
  locals = locals

newFrame :: Temp.Label -> Temp.Generator -> [Frame.EscapesOrNot] -> (Temp.Generator, X64Frame)
newFrame frameName gen escapes =
  let
    (initialFrame, gen') = freshFrame frameName gen
    (gen'', frame, _) = foldl' step (gen', initialFrame, 0) escapes
  in
    (gen'', frame)
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
