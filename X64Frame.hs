{-# LANGUAGE TypeFamilies #-}

module X64Frame where

import qualified Frame
import qualified Temp

import Data.List


data X64Access = InFrame Int | InReg Temp.Label

{-
The first six integer or pointer arguments are passed in registers:
RDI, RSI, RDX, RCX, R8, R9
-}
data X64Frame = X64Frame {name :: Temp.Label,
                          formals :: [X64Access],
                          locals :: [X64Access]}

maxNumRegisterParams :: Int
maxNumRegisterParams = 6

wordSize :: Int
wordSize = 8

numFormalsInReg :: X64Frame -> Int
numFormalsInReg frame =
  foldl' step (0 :: Int) (formals frame)
  where
    step :: Int -> X64Access -> Int
    step ct (InFrame _) = ct
    step ct (InReg _) = ct + 1

freshFrame :: Temp.Label -> X64Frame
freshFrame frameName = X64Frame{name=frameName,
                                formals=[],
                                locals=[]}

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
    initialFrame = freshFrame frameName
  in
    foldl' step (gen, initialFrame) escapes
  where
    step :: (Temp.Generator, X64Frame) -> Frame.EscapesOrNot -> (Temp.Generator, X64Frame)
    step (gen', frame) escapesOrNot = allocFormal gen' frame escapesOrNot
    allocFormal :: Temp.Generator -> X64Frame -> Frame.EscapesOrNot
      -> (Temp.Generator, X64Frame)
    allocFormal gen' frame escapesOrNot =
        if not (Frame.escapes escapesOrNot) &&
           numFormalsInReg frame < maxNumRegisterParams then
          let
            (regLabel, gen'') = Temp.newlabel gen'
          in
            (gen'', frame{formals=(formals frame) ++ [InReg regLabel]})
        else
          let
            (idx, gen'') = Temp.newtemp gen'
          in
            (gen'', frame{formals=(formals frame) ++ [InFrame idx]})

allocLocal :: Temp.Generator -> X64Frame -> Frame.EscapesOrNot
  -> (Temp.Generator, X64Frame, X64Access)
allocLocal gen frame escapesOrNot =
  if Frame.escapes escapesOrNot then
    let
      (idx, gen') = Temp.newtemp gen
      access = InFrame idx
    in
      (gen', frame{locals=(locals frame) ++ [access]}, access)
  else
    let
      (regLabel, gen') = Temp.newlabel gen
      access = InReg regLabel
    in
      (gen', frame{locals=(locals frame) ++ [access]}, access)
