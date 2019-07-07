{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Translate where

import qualified Temp
import qualified Frame

newtype Exp = Exp () deriving (Show)

class Translator f where
  type Level f :: *
  type Access f :: *
  outermost :: Level f
  newLevel :: (Level f, Temp.Label, [Frame.EscapesOrNot]) -> Level f
  formals :: Level f -> [Access f]
  allocLocal :: Level f -> Temp.Generator -> Frame.EscapesOrNot
    -> (Temp.Generator, Access f)
