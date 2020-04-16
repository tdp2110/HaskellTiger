module Types where

import Symbol

import Data.List

type TypeId = Integer

data Ty = INT
  | STRING
  | RECORD ([(Symbol, Ty)], TypeId)
  | ARRAY (Ty, TypeId)
  | NIL
  | UNIT
  | NAME (Symbol, Maybe Ty)

instance Eq Ty where
  INT == INT = True
  STRING == STRING = True
  RECORD(_,typeId) == RECORD(_,typeId') = typeId == typeId'
  ARRAY(_,typeId) == ARRAY(_,typeId') = typeId == typeId'
  NIL == NIL = True
  UNIT == UNIT = True
  NAME(sym,maybeTy) == NAME(sym',maybeTy') = (sym == sym') &&
                                             (maybeTy == maybeTy')
  _ == _ = False

instance Show Ty where
  show INT = "INT"
  show STRING = "STRING"
  show (RECORD(fieldMap,_)) = "RECORD{" ++ showFieldTys fieldMap ++ "}"
  show (ARRAY(typ,_)) = "ARRAY [" ++ shortTyName typ ++ "]"
  show NIL = "NIL"
  show UNIT = "UNIT"
  show (NAME(sym,_)) = "NAME(" ++ show sym ++ ")"

shortTyName :: Ty -> String
shortTyName (RECORD _) = "RECORD{ ... }"
shortTyName (ARRAY _) = "ARRAY [...]"
shortTyName ty = show ty

showFieldTys :: [(Symbol, Ty)] -> String
showFieldTys fieldMap = intercalate "," $
  fmap show [(sym, shortTyName ty) | (sym, ty) <- fieldMap]
