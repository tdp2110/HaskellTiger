{-# LANGUAGE TypeFamilies #-}

module Frame where

import qualified Temp

data EscapesOrNot = Escapes | NoEscape
  deriving (Show)

escapes :: EscapesOrNot -> Bool
escapes Escapes = True
escapes _ = False

class Frame f where
  type Access f :: *
  type Arch f :: *
  newFrame :: (Arch f) -> Temp.Label -> Temp.Generator -> [EscapesOrNot] -> (Temp.Generator, f)
  name :: f -> Temp.Label
  allocLocal :: Temp.Generator -> f -> EscapesOrNot -> (Temp.Generator, f, Access f)
  formals :: f -> [Access f]
  locals :: f -> [Access f]
  rv :: f -> Int
  fp :: f -> Int
