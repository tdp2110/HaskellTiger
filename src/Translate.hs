{-# LANGUAGE TypeFamilies #-}

module Translate where

import qualified Frame
import qualified Temp
import qualified Tree
import qualified X64Frame

import Prelude hiding (exp)


class Translate f where
  type Level f :: *
  type Access f :: *
  outermost :: f -> Level f
  newLevel :: f -> (Level f, Temp.Label, [Frame.EscapesOrNot]) -> Temp.Generator
    -> (Temp.Generator, Level f)
  formals :: f -> Level f -> [Access f]
  allocLocal :: f -> Level f -> Temp.Generator -> Frame.EscapesOrNot
    -> (Temp.Generator, Level f, Access f)

data X64Translate = X64Translate
  deriving (Show)
data X64Level = X64Level { x64Parent :: X64Level
                         , x64Name :: Temp.Label
                         , x64Formals :: [Frame.EscapesOrNot]
                         , x64Frame :: X64Frame.X64Frame}
           | X64Outermost
  deriving (Show)
data X64Access = X64Access { level :: X64Level
                           , access :: X64Frame.X64Access }
  deriving (Show)

instance Translate X64Translate where
  type (Level X64Translate) = X64Level
  type (Access X64Translate) = X64Access
  outermost _ = X64Outermost
  newLevel _ = x64NewLevel
  formals _ lev = x64TranslateFormals lev
  allocLocal _ = x64AllocLocal

x64NewLevel :: (X64Level, Temp.Label, [Frame.EscapesOrNot]) -> Temp.Generator
  -> (Temp.Generator, X64Level)
x64TranslateFormals :: X64Level -> [X64Access]
x64AllocLocal :: X64Level -> Temp.Generator -> Frame.EscapesOrNot ->
  (Temp.Generator, X64Level, X64Access)

x64NewLevel (parent, label, escapes) gen =
  let
    escapes' = [Frame.Escapes] ++ escapes -- initial escape for static link
    (frameLabel, gen') = Temp.newlabel gen
    (gen'', frame') = X64Frame.newFrame frameLabel gen' escapes'
  in
    (gen'', X64Level{ x64Parent=parent
                    , x64Name=label
                    , x64Formals=escapes'
                    , x64Frame=frame' })

x64TranslateFormals lev =
  let
    frameAccesses = Frame.formals (x64Frame lev)
    toTranslateAccess = \frameAccess -> X64Access{ level=lev
                                                 , access=frameAccess }
  in
    map toTranslateAccess frameAccesses

x64AllocLocal lev gen escapeOrNot =
  let
    (gen', frame', access') = X64Frame.allocLocal gen (x64Frame lev) escapeOrNot
    lev' = lev{ x64Frame=frame' }
  in
    (gen', lev', X64Access{level=lev', access=access'})

data Exp =
    Ex Tree.Exp
  | Nx Tree.Stm
  | Cx (Temp.Label -> Temp.Label -> Tree.Stm)

instance Show Exp where
  show (Ex exp) = "Exp.Ex " ++ show exp
  show (Nx stm) = "Exp.Nx " ++ show stm
  show (Cx _) = "Exp.Cx(...)"
