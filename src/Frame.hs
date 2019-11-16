{-# LANGUAGE TypeFamilies #-}

module Frame where

import qualified Absyn
import qualified Symbol
import qualified Temp

data EscapesOrNot = Escapes | NoEscape
  deriving (Eq, Show)

escapes :: EscapesOrNot -> Bool
escapes Escapes = True
escapes _ = False

class Frame f where
  type Access f :: *
  type Arch f :: *
  newFrame :: (Arch f)
              -> Temp.Label
              -> Maybe (Symbol.Symbol, Absyn.Pos)
              -> Temp.Generator
              -> [EscapesOrNot]
              -> (Temp.Generator, f)
  name :: f -> Temp.Label
  allocLocal :: Temp.Generator -> f -> EscapesOrNot -> (Temp.Generator, f, Access f)
  formals :: f -> [Access f]
  locals :: f -> [Access f]
  rv :: f -> Int
  fp :: f -> Int
