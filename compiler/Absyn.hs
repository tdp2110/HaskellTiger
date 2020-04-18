module Absyn where

import           Symbol

data Pos = Pos { absChrOffset :: !Int
               , lineno :: !Int
               , colno :: !Int}
  deriving (Eq)

instance Show Pos where
  show (Pos _ lineNo colNo) =
    "line " ++ (show lineNo ++ ", col " ++ show colNo)

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
         deriving (Eq, Show)

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String
         | CallExp { func :: Symbol
                   , args :: [Exp]
                   , pos :: Pos }
         | OpExp { left :: Exp
                 , oper :: Oper
                 , right :: Exp
                 , pos :: Pos }
         | RecordExp { fields :: [(Symbol, Exp, Pos)]
                     , typ :: Symbol
                     , pos :: Pos }
         | SeqExp [(Exp, Pos)]
         | AssignExp { var :: Var
                     , exp :: Exp
                     , pos :: Pos }
         | IfExp { test :: Exp
                 , then' :: Exp
                 , else' :: Maybe Exp
                 , pos :: Pos }
         | WhileExp { test :: Exp
                    , body :: Exp
                    , pos :: Pos }
         | ForExp { forVar :: Symbol
                  , escape :: Bool
                  , lo :: Exp
                  , hi :: Exp
                  , body :: Exp
                  , pos :: Pos }
         | BreakExp Pos
         | LetExp { decs :: [Dec]
                  , body :: Exp
                  , pos :: Pos }
         | ArrayExp { typ :: Symbol
                    , size :: Exp
                    , init :: Exp
                    , pos :: Pos }
         deriving (Eq, Show)

data Dec = FunctionDec [FunDec]
         | VarDec { name :: Symbol
                  , vardecEscape :: Bool
                  , varDecTyp :: Maybe (Symbol, Pos)
                  , decInit :: Exp
                  , decPos :: Pos }
         | TypeDec [TyDec]
         deriving (Eq, Show)

data Ty = NameTy (Symbol, Pos)
        | RecordTy [Field]
        | ArrayTy (Symbol, Pos)
        deriving (Eq, Show)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
          deriving (Eq, Show)

data Field = Field { fieldName :: Symbol
                   , fieldEscape :: Bool
                   , fieldTyp :: Symbol
                   , fieldPos :: Pos }
           deriving (Eq, Show)

data FunDec = FunDec { fundecName :: Symbol
                     , params :: [Field]
                     , result :: Maybe (Symbol, Pos)
                     , funBody :: Exp
                     , funPos :: Pos }
            deriving (Eq, Show)

data TyDec = TyDec { tydecName :: Symbol
                   , ty :: Ty
                   , tydecPos :: Pos }
            deriving (Eq, Show)
