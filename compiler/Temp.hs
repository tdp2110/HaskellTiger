module Temp
  ( Label(..)
  , Generator(..)
  , newtemp
  , newlabel
  , newGen
  , name
  )
where

import qualified Symbol
import qualified Data.Text                     as T

newtype Label = Label Symbol.Symbol
  deriving (Eq, Ord, Show)

name :: Label -> T.Text
name (Label s) = Symbol.name s

data Generator = Generator {tempIdx :: Int, labelIdx :: Int}

newGen :: Generator
newGen = Generator { tempIdx = 0, labelIdx = 0 }

newtemp :: Generator -> (Int, Generator)
newtemp gen@(Generator idx _) = (idx, gen { tempIdx = idx + 1 })

newlabel :: Generator -> (Label, Generator)
newlabel gen@(Generator _ idx) =
  ( Label $ Symbol.Symbol . T.pack $ ".L" ++ show idx
  , gen { labelIdx = idx + 1 }
  )
