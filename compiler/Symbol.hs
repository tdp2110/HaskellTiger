module Symbol
  ( Symbol(..)
  , name
  )
where

newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol s) = s

name :: Symbol -> String
name (Symbol s) = s
