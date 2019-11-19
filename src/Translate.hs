{-# LANGUAGE TypeFamilies #-}

module Translate where

import qualified Absyn
import qualified Frame
import qualified Symbol
import qualified Temp
import qualified Tree
import qualified X64Frame

import Control.Monad.Trans.State (State, get, put, runState)
import Control.Monad (mapM)
import Data.List
import Prelude hiding (exp)


class Translate t where
  type Level t :: *
  type Access t :: *
  outermost :: t -> Level t
  newLevel :: t
              -> Maybe (Symbol.Symbol, Absyn.Pos)
              -> (Level t, Temp.Label, [Frame.EscapesOrNot])
              -> Temp.Generator
              -> (Temp.Generator, Level t)
  formals :: t -> Level t -> [Access t]
  allocLocal :: t -> Level t -> Temp.Generator -> Frame.EscapesOrNot
    -> (Temp.Generator, Level t, Access t)

data X64Translate = X64Translate { x64 :: X64Frame.X64 }
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
  newLevel translate = x64NewLevel $ x64 translate
  formals _ lev = x64TranslateFormals lev
  allocLocal _ = x64AllocLocal

x64NewLevel :: X64Frame.X64
               -> Maybe (Symbol.Symbol, Absyn.Pos)
               -> (X64Level, Temp.Label, [Frame.EscapesOrNot])
               -> Temp.Generator
               -> (Temp.Generator, X64Level)
x64TranslateFormals :: X64Level -> [X64Access]
x64AllocLocal :: X64Level -> Temp.Generator -> Frame.EscapesOrNot ->
  (Temp.Generator, X64Level, X64Access)

x64NewLevel x64Inst maybeDebug (parent, label, escapes) gen =
  let
    escapes' = [Frame.Escapes] ++ escapes -- initial escape for static link
    (frameLabel, gen') = getLabel
    (gen'', frame') = X64Frame.newFrame
                        x64Inst
                        frameLabel
                        maybeDebug
                        gen'
                        escapes'
    (identity, gen''') = Temp.newtemp gen''
  in
    (gen''', X64Level { x64Parent=parent
                      , x64Name=label
                      , x64Formals=escapes'
                      , x64Frame=frame'
                      , identifier=identity })
  where
    getLabel :: (Temp.Label, Temp.Generator)
    getLabel =
      case maybeDebug of
        Just (sym@(Symbol.Symbol "__tiger_main"), _) -> (Temp.Label sym, gen)
        _ -> Temp.newlabel gen

x64TranslateFormals lev =
  let
    frameAccesses = Frame.formals (x64Frame lev)
    toTranslateAccess = \frameAccess -> X64Access{ level=lev
                                                 , access=frameAccess }
  in
    fmap toTranslateAccess frameAccesses

x64AllocLocal lev gen escapeOrNot =
  let
    (gen', frame', access') = X64Frame.allocLocal gen (x64Frame lev) escapeOrNot
    lev' = lev{ x64Frame=frame' }
  in
    (gen', lev', X64Access{level=lev', access=access'})

data Exp =
    Ex Tree.Exp
  | Nx Tree.Stm
  | Cx CxFun

type CxFun = Temp.Label -> Temp.Label -> Tree.Stm

instance Show Exp where
  show (Ex exp) = "Exp.Ex " ++ show exp
  show (Nx stm) = "Exp.Nx " ++ show stm
  show (Cx _) = "Exp.Cx(...)"

zero :: Tree.Exp
zero = Tree.CONST 0

zeroExp :: Exp
zeroExp = Ex zero

passStm :: Tree.Stm
passStm = Tree.EXP zero

unEx :: Exp -> Temp.Generator -> (Tree.Exp, Temp.Generator)
unEx (Ex exp) gen = (exp, gen)
unEx (Nx stm) gen = (Tree.ESEQ(stm, zero), gen)
unEx (Cx genstm) gen =
  let
    (r, gen') = Temp.newtemp gen
    (t, gen'') = Temp.newlabel gen'
    (f, gen''') = Temp.newlabel gen''
    expRes = Tree.ESEQ(
      Tree.makeSeq [ Tree.MOVE (Tree.TEMP r, Tree.CONST 1)
                   , genstm t f
                   , Tree.LABEL f
                   , Tree.MOVE (Tree.TEMP r, zero)
                   , Tree.LABEL t],
      Tree.TEMP r)
  in
    (expRes, gen''')

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
    stmtRes = Tree.makeSeq [genstm t f, Tree.LABEL t, Tree.LABEL f]
  in
    (stmtRes, gen'')

simpleVar :: X64Access -> X64Level -> Exp
simpleVar X64Access{level=declaredLevel, access=accessInDeclaredFrame} levelAtUse =
  Ex $ X64Frame.exp accessInDeclaredFrame $ staticLink levelAtUse declaredLevel

ptrCmp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
ptrCmp r1 r2 op gen =
  if isEqualityOper then
    relOp r1 r2 op gen
  else
    error "shouldn't get here"
  where
    isEqualityOper = op == Absyn.EqOp || op == Absyn.NeqOp

stringCmp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
stringCmp s1 s2 op gen =
  let
    (str1, gen') = unEx s1 gen
    (str2, gen'') = unEx s2 gen'
    cmpExp = X64Frame.externalCall
             (Temp.Label $ Symbol.Symbol "__tiger_strCmp")
             [str1, str2]
    treeOp = transRelOp op
    resExp = Cx $ \t f ->
      Tree.CJUMP ( treeOp
                 , cmpExp
                 , zero
                 , t
                 , f )
  in
    (resExp, gen'')

record :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
record exps gen =
  let
    (r, gen') = Temp.newtemp gen
    rExp = Tree.TEMP r
    numExps = length exps
    wordSize = X64Frame.wordSize
    mallocStm = Tree.MOVE ( rExp
                          , X64Frame.externalCall
                              (Temp.Label $ Symbol.Symbol "__tiger_alloc")
                              [Tree.CONST $ numExps * wordSize] )
    (initStm, gen'') = foldl'
                       step
                       (passStm, gen')
                       $ zip exps [1 :: Int ..]
    step :: (Tree.Stm, Temp.Generator) -> (Exp, Int) -> (Tree.Stm, Temp.Generator)
    step (stm, g) (exp, idx) =
      let
        (expr, g') = unEx exp g
        memExpr = Tree.MEM $ Tree.BINOP (Tree.PLUS, rExp,  Tree.CONST $ idx * wordSize)
      in
        ( Tree.SEQ ( stm
                   , Tree.MOVE (memExpr, expr))
        , g' )
    resExp = Ex $ Tree.ESEQ ( Tree.SEQ ( mallocStm
                                       , initStm )
                            , rExp)
  in
    (resExp, gen'')

array :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
array sizeExp initExpr gen =
  let
    (r, gen') = Temp.newtemp gen
    (sizeExpr, gen'') = unEx sizeExp gen'
    (initExpr', gen''') = unEx initExpr gen''
    rExp = Tree.TEMP r
    initStm = Tree.MOVE ( rExp
                        , X64Frame.externalCall
                          (Temp.Label $ Symbol.Symbol "__tiger_initArray")
                          [sizeExpr, initExpr'] )
    resExp = Ex $ Tree.ESEQ (initStm, rExp)
  in
    (resExp, gen''')

relOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
relOp expLeft expRight op gen =
  let
    (expLeft', gen') = unEx expLeft gen
    (expRight', gen'') = unEx expRight gen'
    treeOp = transRelOp op
    resExp = Cx $ \t f -> Tree.CJUMP (treeOp, expLeft', expRight', t, f)
  in
    (resExp, gen'')

transRelOp :: Absyn.Oper -> Tree.Relop
transRelOp op = case op of
                  Absyn.EqOp -> Tree.EQ
                  Absyn.NeqOp -> Tree.NE
                  Absyn.LtOp -> Tree.LT
                  Absyn.LeOp -> Tree.LE
                  Absyn.GtOp -> Tree.GT
                  Absyn.GeOp -> Tree.GE
                  _ -> error "shouldn't get here"

binOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
binOp expLeft expRight op gen =
  let
    (expLeft', gen') = unEx expLeft gen
    (expRight', gen'') = unEx expRight gen'
    op' = case op of
            Absyn.PlusOp -> Tree.PLUS
            Absyn.MinusOp -> Tree.MINUS
            Absyn.TimesOp -> Tree.MUL
            Absyn.DivideOp -> Tree.DIV -- TODO! check for division by zero!
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
    resExp = Nx $ Tree.makeSeq [ cx t f
                               , Tree.LABEL t
                               , thenStm
                               , Tree.LABEL f ]
  in
    (resExp, gen''')

seqExp :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
seqExp [] gen = (Ex zero, gen)
seqExp (exp:exps) gen =
  let
    (headStm, gen') = unNx exp gen
    (tailExp, gen'') = seqExp exps gen'
    (tailExp', gen''') = unEx tailExp gen''
    resExp = Ex $ Tree.ESEQ (headStm, tailExp')
  in
    (resExp, gen''')

seqStm :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
seqStm [] gen = (Ex zero, gen)
seqStm (exp:exps) gen =
  let
    (headStm, gen') = unNx exp gen
    (tailExp, gen'') = seqExp exps gen'
    (tailStm, gen''') = unNx tailExp gen''
    resStm = Nx $ Tree.SEQ (headStm, tailStm)
  in
    (resStm, gen''')

{-
--TODO: I'd like to DRY the last two functions with something like below,
but it doesn't quite compile

data StmOrExp = IsStm | IsExp

seqExpOrStm :: StmOrExp -> [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
seqStm seqOrStm [] gen =
  case seqOrStm of
    IsStm -> (Nx zero, gen)
    IsExp -> (Ex zerp, gen)
seqStm seqOrStm (exp:exps) gen =
  let
    (headExpOrStm, gen') = unNx exp gen
    (tailExpOrStm, gen'') = seqExpOrStm seqOrStm exps gen'
    unFn = case seqOrStm of
             IsStm -> unNx
             IsExp -> unEx
    (tailExpOrStm', gen''') = unFn tailExpOrStm gen''
    res = case seqOrStm of
            IsStm -> Nx $ Tree.SEQ (headExpOrStm, tailExpOrStm)
            IsExp -> Ex $ Tree.ESEQ (headExpOrStm, tailExpOrStm)
  in
    (res, gen''')
-}

ifThenElseStm :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThenElseStm testExpE thenExp elseExp gen =
  let
    testGen = unCx testExpE
    (t, gen') = Temp.newlabel gen
    (f, gen'') = Temp.newlabel gen'
    (joinLab, gen''') = Temp.newlabel gen''
    (thenStm, gen4) = unNx thenExp gen'''
    (elseStm, gen5) = unNx elseExp gen4
    resExp = Nx $ Tree.makeSeq [ testGen t f
                               , Tree.LABEL t
                               , thenStm
                               , Tree.JUMP (Tree.NAME joinLab, [joinLab])
                               , Tree.LABEL f
                               , elseStm
                               , Tree.LABEL joinLab ]
  in
    (resExp, gen5)

ifThenElseCx :: CxFun -> CxFun -> CxFun -> Temp.Generator -> (Exp, Temp.Generator)
ifThenElseCx testGen thenGen elseGen gen =
  let
    (z, gen') = Temp.newlabel gen
    (w, gen'') = Temp.newlabel gen'
    resExp = Cx $ \t f -> Tree.makeSeq [ testGen z f
                                      , Tree.LABEL z
                                      , thenGen w f
                                      , Tree.LABEL w
                                      , elseGen t f ]
  in
    (resExp, gen'')

ifThenElse :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThenElse testExpE (Cx thenGen) elseE gen =
  let
    testGen = unCx testExpE
    elseGen = unCx elseE
  in
    ifThenElseCx testGen thenGen elseGen gen
ifThenElse testExpE thenE (Cx elseGen) gen =
  let
    testGen = unCx testExpE
    thenGen = unCx thenE
  in
    ifThenElseCx testGen thenGen elseGen gen
ifThenElse testExpE thenExpE elseExpE gen =
  let
    testGen = unCx testExpE
    (thenExp, gen') = unEx thenExpE gen
    (elseExp, gen'') = unEx elseExpE gen'
    (t, gen''') = Temp.newlabel gen''
    (f, gen4) = Temp.newlabel gen'''
    (joinLab, gen5) = Temp.newlabel gen4
    (r, gen6) = Temp.newtemp gen5
    resExp = Ex $ Tree.ESEQ (Tree.makeSeq [ testGen t f
                                          , Tree.LABEL t
                                          , Tree.MOVE (Tree.TEMP r, thenExp)
                                          , Tree.JUMP (Tree.NAME joinLab, [joinLab])
                                          , Tree.LABEL f
                                          , Tree.MOVE (Tree.TEMP r, elseExp)
                                          , Tree.LABEL joinLab ]
                            , Tree.TEMP r)
  in
    (resExp, gen6)

assign :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
assign lhs rhs gen =
  let
    (lhsExp, gen') = unEx lhs gen
    (rhsExp, gen'') = unEx rhs gen'
    resExp = Nx $ Tree.MOVE (lhsExp, rhsExp)
  in
    (resExp, gen'')

letExpM :: [Exp] -> Exp -> State Temp.Generator Exp
letExpM initializers bodyExp = do
  gen <- get
  let
    (body, gen') = unEx bodyExp gen
    (initializerStms, gen'') = runState (mapM unNxM initializers) gen'
    in do
    put gen''
    pure $ Ex $ Tree.ESEQ (Tree.makeSeq initializerStms, body)

letExp :: [Exp] -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
letExp initializers body gen =
  runState (letExpM initializers body) gen

while :: Exp -> Exp -> Temp.Label -> Temp.Generator -> (Exp, Temp.Generator)
while testExpE bodyExpE doneLab gen =
  let
    (testLab, gen') = Temp.newlabel gen
    (bodyLab, gen'') = Temp.newlabel gen'
    (testExp, gen''') = unEx testExpE gen''
    (bodyExp, gen4) = unEx bodyExpE gen'''
    resExp = Nx $ Tree.makeSeq [ Tree.LABEL testLab
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
    happyPath = Ex $ Tree.MEM $ Tree.BINOP
                ( Tree.PLUS
                , recordExp
                , Tree.CONST $ fieldNumber * X64Frame.wordSize )
    sadPath = Ex $ X64Frame.externalCall
              (Temp.Label $ Symbol.Symbol "__tiger_nullRecordDereference")
              []
    jumpExp = Cx $ \happyLab sadLab ->
      Tree.CJUMP (Tree.NE, recordExp, zero, happyLab, sadLab)
  in
    ifThenElse jumpExp happyPath sadPath gen'

subscript :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
subscript arrExpE indexExpE gen =
  let
    (arrExp, gen') = unEx arrExpE gen
    (indexExp, gen'') = unEx indexExpE gen'
    sizeExp = Tree.MEM arrExp
    wordSize = Tree.CONST $ X64Frame.wordSize
    happyPath = Ex $ Tree.MEM $ Tree.BINOP
                ( Tree.PLUS
                , arrExp
                , Tree.BINOP ( Tree.PLUS
                             , Tree.BINOP ( Tree.MUL
                                          , wordSize
                                          , indexExp )
                             , wordSize ) )
    sadPath = Ex $ X64Frame.externalCall
              (Temp.Label $ Symbol.Symbol "__tiger_indexError")
              [sizeExp, indexExp]
    (gtZeroExp, gen3) = relOp indexExpE zeroExp Absyn.GeOp gen''
    (gtZero, gen4) = unEx gtZeroExp gen3
    (ltSizeExp, gen5) = relOp indexExpE (Ex sizeExp) Absyn.LtOp gen4
    (ltSize, gen6) = unEx ltSizeExp gen5
    testExp = Tree.BINOP (Tree.AND, gtZero, ltSize)
    jumpExp = Cx $ \happyLab sadLab ->
      Tree.CJUMP (Tree.NE, testExp, zero, happyLab, sadLab)
  in
    ifThenElse jumpExp happyPath sadPath gen6

type Frag = X64Frame.Frag

string :: String -> Temp.Generator -> (Exp, Frag, Temp.Generator)
string str gen =
  let
    (label, gen') = Temp.newlabel gen
    (r, gen'') = Temp.newtemp gen'
    sExp = Tree.TEMP r
    resExp = Ex $ Tree.ESEQ (
                    Tree.MOVE ( sExp
                              , X64Frame.externalCall
                                  (Temp.Label $ Symbol.Symbol "__tiger_allocString")
                                  [Tree.NAME(label), Tree.CONST $ length str])
                  , sExp )
    frag = X64Frame.STRING (label, str)
  in
    (resExp, frag, gen'')

unExM :: Exp -> State Temp.Generator Tree.Exp
unExM exp = do
  gen <- get
  let
    (treeExp, gen') = unEx exp gen
    in do
    put gen'
    pure treeExp

-- TODO: how to dry up unExM and unNxM? C++ templates (with duck-typing) could do it ...
unNxM :: Exp -> State Temp.Generator Tree.Stm
unNxM exp = do
  gen <- get
  let
    (treeStm, gen') = unNx exp gen
    in do
    put gen'
    pure treeStm

callM :: X64Level -> X64Level -> Temp.Label -> [Exp] -> State Temp.Generator Exp
callM funLevel callerLevel funlab params = do
  gen <- get
  let
    (treeParams, gen') = runState (mapM unExM params) gen
    escapes = x64Formals funLevel
    in do
    put gen'
    pure $ Ex $ case x64Parent funLevel of
                    X64Outermost ->
                      X64Frame.externalCall funlab treeParams
                    funParentLevel ->
                      Tree.CALL ( Tree.NAME funlab
                                , [staticLink
                                    callerLevel
                                    funParentLevel]
                                  ++ treeParams
                                , escapes)

call :: X64Level -> X64Level -> Temp.Label -> [Exp] -> Temp.Generator
  -> (Exp, Temp.Generator)
call funLevel callerLevel funlab params gen =
  runState (callM funLevel callerLevel funlab params) gen

nilexp :: Exp
nilexp = Ex $ Tree.CONST 0

intexp :: Int -> Exp
intexp i = Ex $ Tree.CONST i

initExp :: X64Access -> X64Level -> Exp -> Temp.Generator
           -> (Exp, Temp.Generator)
initExp acc lev exp gen =
  assign (simpleVar acc lev) exp gen

functionDec :: X64Level -> Exp -> Temp.Generator -> (Frag, Temp.Generator)
functionDec X64Level{x64Frame=frame} bodyExp gen =
  let
    (bodyExpr, gen') = unEx bodyExp gen
    (bodyExpr', gen'') = X64Frame.procEntryExit1 frame bodyExpr gen'
    bodyStm = Tree.MOVE ( Tree.TEMP $ Frame.rv frame
                        , bodyExpr' )
  in
    (makeProc bodyStm, gen'')
  where
    makeProc :: Tree.Stm -> Frag
    makeProc bodyStm =
      let
        resBody = Tree.SEQ (Tree.LABEL $ X64Frame.name frame, bodyStm)
      in
        X64Frame.PROC { X64Frame.body=resBody
                      , X64Frame.fragFrame=frame }
functionDec X64Outermost _ _ = error "should not get here"


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
