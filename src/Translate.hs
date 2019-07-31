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
                         , x64Frame :: X64Frame.X64Frame
                         , identifier :: Int }
           | X64Outermost
  deriving (Show)
data X64Access = X64Access { level :: X64Level
                           , access :: X64Frame.X64Access }
  deriving (Show)

instance Eq X64Level where
  X64Outermost == X64Outermost = True
  (X64Level _ _ _ _ id1) == (X64Level _ _ _ _ id2) = id1 == id2
  _ == _ = False

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
    (identity, gen''') = Temp.newtemp gen''
  in
    (gen''', X64Level{ x64Parent=parent
                     , x64Name=label
                     , x64Formals=escapes'
                     , x64Frame=frame'
                     , identifier=identity })

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

unEx :: Exp -> Temp.Generator -> (Tree.Exp, Temp.Generator)
unEx (Ex exp) gen = (exp, gen)
unEx (Nx stm) gen = (Tree.ESEQ(stm, Tree.CONST 0), gen)
unEx (Cx genstm) gen =
  let
    (r, gen') = Temp.newtemp gen
    (t, gen'') = Temp.newlabel gen'
    (f, gen''') = Temp.newlabel gen''
    expRes = Tree.ESEQ(
      makeSeq [ Tree.MOVE(Tree.TEMP r, Tree.CONST 1)
          , genstm t f
          , Tree.LABEL f
          , Tree.MOVE(Tree.TEMP r, Tree.CONST 0)
          , Tree.LABEL t],
      Tree.TEMP r)
  in
    (expRes, gen''')

makeSeq :: [Tree.Stm] -> Tree.Stm
makeSeq [] = Tree.EXP $ Tree.CONST 0
makeSeq (stmt:stmts) = Tree.SEQ(stmt, makeSeq stmts)

unCx :: Exp -> (Temp.Label -> Temp.Label -> Tree.Stm)
unCx (Ex (Tree.CONST 0)) = \_ f -> Tree.JUMP(Tree.NAME f, [f])
unCx (Ex (Tree.CONST 1)) = \t _ -> Tree.JUMP(Tree.NAME t, [t])
unCx (Ex exp) = \t f -> Tree.CJUMP (Tree.NE, exp, Tree.CONST 0, t, f)
unCx (Cx genstm) = genstm
unCx (Nx _) = error "should never get here"

unNx :: Exp -> Temp.Generator -> (Tree.Stm, Temp.Generator)
unNx (Nx stm) gen = (stm, gen)
unNx (Ex exp) gen = (Tree.EXP exp, gen)
unNx (Cx genstm) gen =
  let
    (t, gen') = Temp.newlabel gen
    (f, gen'') = Temp.newlabel gen'
    stmtRes = makeSeq [genstm t f, Tree.LABEL t, Tree.LABEL f]
  in
    (stmtRes, gen'')

simpleVar :: X64Access -> X64Level -> Exp
simpleVar X64Access{level=declaredLevel, access=accessInDeclaredFrame} levelAtUse =
  Ex . go levelAtUse $ X64Frame.frameExp $ x64Frame levelAtUse
  where
    go :: X64Level -> Tree.Exp -> Tree.Exp
    go currentLevel currentFPExp =
      if declaredLevel == currentLevel then
        X64Frame.exp accessInDeclaredFrame currentFPExp
        else
        go (x64Parent currentLevel) $ X64Frame.staticLink currentFPExp
