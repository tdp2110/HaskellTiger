{-# LANGUAGE TypeFamilies #-}

module Translate where

import qualified Absyn
import qualified Frame
import qualified Temp
import qualified Tree
import qualified X64Frame

import Control.Monad.Trans.State (State, get, put, runState)
import Control.Monad (mapM)
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

zero :: Tree.Exp
zero = Tree.CONST 0

unEx :: Exp -> Temp.Generator -> (Tree.Exp, Temp.Generator)
unEx (Ex exp) gen = (exp, gen)
unEx (Nx stm) gen = (Tree.ESEQ(stm, zero), gen)
unEx (Cx genstm) gen =
  let
    (r, gen') = Temp.newtemp gen
    (t, gen'') = Temp.newlabel gen'
    (f, gen''') = Temp.newlabel gen''
    expRes = Tree.ESEQ(
      makeSeq [ Tree.MOVE(Tree.TEMP r, Tree.CONST 1)
              , genstm t f
              , Tree.LABEL f
              , Tree.MOVE(Tree.TEMP r, zero)
              , Tree.LABEL t],
      Tree.TEMP r)
  in
    (expRes, gen''')

makeSeq :: [Tree.Stm] -> Tree.Stm
makeSeq [] = Tree.EXP $ zero
makeSeq (stmt:stmts) = Tree.SEQ(stmt, makeSeq stmts)

unCx :: Exp -> (Temp.Label -> Temp.Label -> Tree.Stm)
unCx (Ex (Tree.CONST 0)) = \_ f -> Tree.JUMP(Tree.NAME f, [f])
unCx (Ex (Tree.CONST 1)) = \t _ -> Tree.JUMP(Tree.NAME t, [t])
unCx (Ex exp) = \t f -> Tree.CJUMP (Tree.NE, exp, zero, t, f)
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
  Ex $ X64Frame.exp accessInDeclaredFrame $ staticLink levelAtUse declaredLevel

relOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
relOp expLeft expRight op gen =
  let
    (expLeft', gen') = unEx expLeft gen
    (expRight', gen'') = unEx expRight gen'
    op' = case op of
            Absyn.EqOp -> Tree.EQ
            Absyn.NeqOp -> Tree.NE
            Absyn.LtOp -> Tree.LT
            Absyn.LeOp -> Tree.LE
            Absyn.GtOp -> Tree.GT
            Absyn.GeOp -> Tree.GE
            _ -> error "shouldn't get here"
    resExp = Cx $ \t f -> Tree.CJUMP (op', expLeft', expRight', t, f)
  in
    (resExp, gen'')

binOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
binOp expLeft expRight op gen =
  let
    (expLeft', gen') = unEx expLeft gen
    (expRight', gen'') = unEx expRight gen'
    op' = case op of
            Absyn.PlusOp -> Tree.PLUS
            Absyn.MinusOp -> Tree.MINUS
            Absyn.TimesOp -> Tree.MUL
            Absyn.DivideOp -> Tree.DIV
            _ -> error "shouldn't get here"
    resExp = Ex $ Tree.BINOP (op', expLeft', expRight')
  in
    (resExp, gen'')

ifThen :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThen testExpE thenExpE gen =
  let
    cx = unCx testExpE
    (thenStm, gen') = unNx thenExpE gen
    (t, gen'') = Temp.newlabel gen'
    (f, gen''') = Temp.newlabel gen''
    resExp = Nx $ makeSeq [ cx t f
                          , Tree.LABEL t
                          , thenStm
                          , Tree.LABEL f ]
  in
    (resExp, gen''')

{-
TODO page 162 notes that this should be optimized
-}
ifThenElse :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThenElse testExpE thenExpE elseExpE gen =
  let
    testGen = unCx testExpE
    (thenExp, gen') = unEx thenExpE gen
    (elseExp, gen'') = unEx elseExpE gen'
    (t, gen''') = Temp.newlabel gen''
    (f, gen4) = Temp.newlabel gen'''
    (joinLab, gen5) = Temp.newlabel gen4
    (r, gen6) = Temp.newtemp gen5
    resExp = Ex $ Tree.ESEQ (makeSeq [testGen t f,
                                      Tree.LABEL t,
                                      Tree.MOVE (Tree.TEMP r, thenExp),
                                      Tree.JUMP (Tree.NAME joinLab, [joinLab]),
                                      Tree.LABEL f,
                                      Tree.MOVE (Tree.TEMP r, elseExp),
                                      Tree.LABEL joinLab]
                            , Tree.TEMP r)
  in
    (resExp, gen6)

while :: Exp -> Exp -> Temp.Label -> Temp.Generator -> (Exp, Temp.Generator)
while testExpE bodyExpE doneLab gen =
  let
    (testLab, gen') = Temp.newlabel gen
    (bodyLab, gen'') = Temp.newlabel gen'
    (testExp, gen''') = unEx testExpE gen''
    (bodyExp, gen4) = unEx bodyExpE gen'''
    resExp = Nx $ makeSeq [ Tree.LABEL testLab
                          , Tree.CJUMP (Tree.NE, zero, testExp, doneLab, bodyLab)
                          , Tree.LABEL bodyLab
                          , Tree.EXP bodyExp
                          , Tree.JUMP (Tree.NAME testLab, [testLab])
                          , Tree.LABEL doneLab ]
  in
    (resExp, gen4)

break :: Temp.Label -> Exp
break breakTarget = Nx $ Tree.JUMP (Tree.NAME breakTarget, [breakTarget])

field :: Exp -> Int -> Temp.Generator -> (Exp, Temp.Generator)
field recordExpE fieldNumber gen =
  let
    (recordExp, gen') = unEx recordExpE gen
    resExp = Ex $ Tree.MEM $ Tree.BINOP
             ( Tree.PLUS
             , recordExp
             , Tree.CONST $ fieldNumber * X64Frame.wordSize )
  in
    (resExp, gen')


subscript :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
subscript arrExpE indexExpE gen =
  let
    (arrExp, gen') = unEx arrExpE gen
    (indexExp, gen'') = unEx indexExpE gen'
    resExp = Ex $ Tree.MEM $ Tree.BINOP
             ( Tree.PLUS
             , arrExp
             , Tree.BINOP (Tree.MUL,
                          Tree.CONST X64Frame.wordSize,
                          indexExp) )
  in
    (resExp, gen'')

unExM :: Exp -> State Temp.Generator Tree.Exp
unExM exp = do
  gen <- get
  let
    (treeExp, gen') = unEx exp gen
    in do
    put gen'
    return treeExp

callM :: X64Level -> X64Level -> Temp.Label -> [Exp] -> State Temp.Generator Exp
callM funLevel callerLevel funlab params = do
  gen <- get
  let
    (treeParams, gen') = runState (mapM unExM params) gen
    in do
    put gen'
    return $ Ex $ case x64Parent funLevel of
                    X64Outermost ->
                      X64Frame.externalCall funlab treeParams
                    funParentLevel ->
                      Tree.CALL ( Tree.NAME funlab
                                , [staticLink
                                    callerLevel
                                    funParentLevel]
                                  ++ treeParams)

call :: X64Level -> X64Level -> Temp.Label -> [Exp] -> Temp.Generator
  -> (Exp, Temp.Generator)
call funLevel callerLevel funlab params gen =
  runState (callM funLevel callerLevel funlab params) gen

staticLink :: X64Level -> X64Level -> Tree.Exp
staticLink X64Outermost _ = error "outermost can't find static links"
staticLink _ X64Outermost = error "can't find static links to outermost"
staticLink baseLevel destLevel =
  let
    baseFrame = x64Frame baseLevel
  in
    if baseLevel == destLevel then
      X64Frame.frameExp baseFrame
    else
      case X64Frame.formals baseFrame of
        [] ->
          error "expected static link in formals"
        (sl : _) ->
          X64Frame.exp sl $ staticLink (x64Parent baseLevel) destLevel
