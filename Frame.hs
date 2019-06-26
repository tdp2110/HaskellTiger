{-# LANGUAGE TypeFamilies #-}

module Frame where

import qualified Temp

class Frame f where
  type Access f :: *
  newFrame :: Temp.Generator -> [Bool] -> (Temp.Generator, f)
  name :: f -> [Access f]
  allocLocal :: Temp.Generator -> f -> Bool -> (Temp.Generator, Access)
