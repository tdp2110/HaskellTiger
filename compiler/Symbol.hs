module Symbol
  ( Symbol(..)
  , name, mkSym
  )
where

import qualified Data.Text as T

newtype Symbol = Symbol T.Text deriving (Eq, Ord)

mkSym :: String -> Symbol
mkSym = Symbol . T.pack

instance Show Symbol where
  show (Symbol s) = show s

name :: Symbol -> T.Text
name (Symbol s) = s
