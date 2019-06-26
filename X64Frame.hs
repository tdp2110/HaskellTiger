module X64Frame where

import qualified Frame
import qualified Temp


data Access = InFrame Int | InReg Temp.Label

{-
The first six integer or pointer arguments are passed in registers:
RDI, RSI, RDX, RCX, R8, R9
-}
data X64Frame = X64Frame {name :: Temp.Label,
                          formals :: [Access],
                          locals :: [Access]}

maxNumRegisterParams :: Int
maxNumRegisterParams = 6

instance Frame.Frame X64Frame where
  Frame.Access = Access
  Frame.newFrame = newFrame
  Frame.name = name
  Frame.allocLocal = allocLocal

remainingArgRegisters :: Frame.Frame -> Int
remainingArgRegisters frame =
  maxNumRegisterParams - (numInReg $ formals frame) - (numInReg $ locals frame)
  where
    numInReg :: [Access] -> Int
    numInReg accesses = sum $ filter isInReg accesses
    isInReg :: Access -> Bool
    isInReg (InReg _) = True
    isInReg _ = False

newFrame :: Temp.Generator -> [Bool] -> (Temp.Generator, f)
newFrame gen accesses =
  let
    doEscape = [access | access <- accesses, access]
    doNotEscape = [access | access <- accesses, not access]
    freshFrame

allocLocal :: Temp.Generator -> Frame.Frame -> Bool -> (Temp.Generator, Frame.Frame, Access)
allocLocal gen frame escapes =
