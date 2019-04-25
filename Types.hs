module Types where

import Symbol

type TypeId = Integer

data Ty = INT
  | STRING
  | RECORD ([(Symbol, Ty)], TypeId)
  | ARRAY (Ty, TypeId)
  | NIL
  | UNIT
  | Name (Symbol, Maybe Ty)
