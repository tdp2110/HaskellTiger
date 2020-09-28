{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Translate
  ( Translate(..)
  , Exp(..)
  , X64Access(..)
  , X64Level(..)
  , X64Translate(..)
  , x64NewLevel
  , initExp
  , functionDec
  , unNx
  , Translate.break
  , array
  , letExp
  , while
  , ifThenElse
  , ifThenElseStm
  , ifThen
  , setField
  , assign
  , assignConst
  , seqExp
  , setitem
  , seqStm
  , record
  , ptrCmp
  , stringCmp
  , relOp
  , call
  , binOp
  , string
  , intexp
  , nilexp
  , subscript
  , simpleVar
  , field
  , x64TranslateFormals
  , x64AllocLocal
  )
where

import qualified Absyn
import qualified Frame
import qualified Symbol
import qualified Temp
import qualified TreeIR
import qualified X64Frame
import qualified Data.Text                     as T

import           Control.Monad.Trans.State      ( State
                                                , get
                                                , put
                                                , runState
                                                )
import           Data.List
import           Prelude                 hiding ( exp )


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

newtype X64Translate = X64Translate { x64 :: X64Frame.X64 }
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
  X64Outermost           == X64Outermost           = True
  (X64Level _ _ _ _ id1) == (X64Level _ _ _ _ id2) = id1 == id2
  _                      == _                      = False

instance Translate X64Translate where
  type Level X64Translate = X64Level
  type Access X64Translate = X64Access
  outermost _ = X64Outermost
  newLevel translate = x64NewLevel $ x64 translate
  formals _ = x64TranslateFormals
  allocLocal _ = x64AllocLocal

x64NewLevel
  :: X64Frame.X64
  -> Maybe (Symbol.Symbol, Absyn.Pos)
  -> (X64Level, Temp.Label, [Frame.EscapesOrNot])
  -> Temp.Generator
  -> (Temp.Generator, X64Level)
x64TranslateFormals :: X64Level -> [X64Access]
x64AllocLocal
  :: X64Level
  -> Temp.Generator
  -> Frame.EscapesOrNot
  -> (Temp.Generator, X64Level, X64Access)

x64NewLevel x64Inst maybeDebug (parent, label, escapes) gen =
  let
    escapes'           = Frame.Escapes : escapes -- initial escape for static link
    (frameLabel, gen') = getLabel
    (gen'', frame') =
      X64Frame.newFrame x64Inst frameLabel maybeDebug gen' escapes'
    (identity, gen''') = Temp.newtemp gen''
  in
    ( gen'''
    , X64Level { x64Parent  = parent
               , x64Name    = label
               , x64Formals = escapes'
               , x64Frame   = frame'
               , identifier = identity
               }
    )
 where
  getLabel :: (Temp.Label, Temp.Generator)
  getLabel = case maybeDebug of
    Just (sym@(Symbol.Symbol "_main"), _) -> (Temp.Label sym, gen)
    _ -> Temp.newlabel gen

x64TranslateFormals lev =
  let frameAccesses = Frame.formals (x64Frame lev)
      toTranslateAccess frameAccess =
          X64Access { level = lev, access = frameAccess }
  in  fmap toTranslateAccess frameAccesses

x64AllocLocal lev gen escapeOrNot =
  let (gen', frame', access') =
          X64Frame.allocLocal gen (x64Frame lev) escapeOrNot
      lev' = lev { x64Frame = frame' }
  in  (gen', lev', X64Access { level = lev', access = access' })

data Exp =
    Ex TreeIR.Exp
  | Nx TreeIR.Stm
  | Cx CxFun

type CxFun = Temp.Label -> Temp.Label -> TreeIR.Stm

instance Show Exp where
  show (Ex exp) = "Exp.Ex " ++ show exp
  show (Nx stm) = "Exp.Nx " ++ show stm
  show (Cx _  ) = "Exp.Cx(...)"

zero :: TreeIR.Exp
zero = TreeIR.CONST 0

zeroExp :: Exp
zeroExp = Ex zero

passStm :: TreeIR.Stm
passStm = TreeIR.EXP zero

unEx :: Exp -> Temp.Generator -> (TreeIR.Exp, Temp.Generator)
unEx (Ex exp) gen = (exp, gen)
unEx (Nx stm) gen = (TreeIR.ESEQ (stm, zero), gen)
unEx (Cx genstm) gen =
  let (r, gen'  ) = Temp.newtemp gen
      (t, gen'' ) = Temp.newlabel gen'
      (f, gen''') = Temp.newlabel gen''
      expRes      = TreeIR.ESEQ
        ( TreeIR.makeSeq
          [ TreeIR.MOVE (TreeIR.TEMP r, TreeIR.CONST 1)
          , genstm t f
          , TreeIR.LABEL (f, Nothing)
          , TreeIR.MOVE (TreeIR.TEMP r, zero)
          , TreeIR.LABEL (t, Nothing)
          ]
        , TreeIR.TEMP r
        )
  in  (expRes, gen''')

unCx :: Exp -> (Temp.Label -> Temp.Label -> TreeIR.Stm)
unCx (Ex (TreeIR.CONST 0)) = \_ f -> TreeIR.JUMP (TreeIR.NAME f, [f])
unCx (Ex (TreeIR.CONST 1)) = \t _ -> TreeIR.JUMP (TreeIR.NAME t, [t])
unCx (Ex exp             ) = \t f -> TreeIR.CJUMP (TreeIR.NE, exp, zero, t, f)
unCx (Cx genstm          ) = genstm
unCx (Nx _               ) = error "should never get here"

unNx :: Exp -> Temp.Generator -> (TreeIR.Stm, Temp.Generator)
unNx (Nx stm) gen = (stm, gen)
unNx (Ex exp) gen = (TreeIR.EXP exp, gen)
unNx (Cx genstm) gen =
  let (t, gen' ) = Temp.newlabel gen
      (f, gen'') = Temp.newlabel gen'
      stmtRes    = TreeIR.makeSeq
        [genstm t f, TreeIR.LABEL (t, Nothing), TreeIR.LABEL (f, Nothing)]
  in  (stmtRes, gen'')

simpleVar :: X64Access -> X64Level -> Exp
simpleVar X64Access { level = declaredLevel, access = accessInDeclaredFrame } levelAtUse
  = Ex $ X64Frame.exp accessInDeclaredFrame $ chaseStaticLinks levelAtUse
                                                               declaredLevel

ptrCmp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
ptrCmp r1 r2 op gen = if isEqualityOper
  then relOp r1 r2 op gen
  else error "shouldn't get here"
  where isEqualityOper = op == Absyn.EqOp || op == Absyn.NeqOp

stringCmp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
stringCmp s1 s2 op gen =
  let (str1, gen' ) = unEx s1 gen
      (str2, gen'') = unEx s2 gen'
      cmpExp        = X64Frame.externalCall
        (Temp.Label $ Symbol.Symbol "tiger_strCmp")
        [str1, str2]
        True
      treeOp = transRelOp op
      resExp = Cx $ \t f -> TreeIR.CJUMP (treeOp, cmpExp, zero, t, f)
  in  (resExp, gen'')

record :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
record exps gen =
  let
    (r, gen') = Temp.newtemp gen
    rExp      = TreeIR.TEMP r
    numExps   = length exps
    wordSize  = X64Frame.wordSize
    mallocStm = TreeIR.MOVE
      ( rExp
      , X64Frame.externalCall (Temp.Label $ Symbol.Symbol "tiger_alloc")
                              [TreeIR.CONST $ numExps * wordSize]
                              True
      )
    (initStm, gen'') = foldl' step (passStm, gen') $ zip exps [0 :: Int ..]
    step
      :: (TreeIR.Stm, Temp.Generator)
      -> (Exp, Int)
      -> (TreeIR.Stm, Temp.Generator)
    step (stm, g) (exp, idx) =
      let (expr, g') = unEx exp g
      in  (TreeIR.SEQ (stm, setRecordField rExp idx expr), g')
    resExp = Ex $ TreeIR.ESEQ (TreeIR.SEQ (mallocStm, initStm), rExp)
  in
    (resExp, gen'')

array :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
array sizeExp initExpr gen =
  let (r        , gen'  ) = Temp.newtemp gen
      (sizeExpr , gen'' ) = unEx sizeExp gen'
      (initExpr', gen''') = unEx initExpr gen''
      rExp                = TreeIR.TEMP r
      initStm             = TreeIR.MOVE
        ( rExp
        , X64Frame.externalCall (Temp.Label $ Symbol.Symbol "tiger_initArray")
                                [sizeExpr, initExpr']
                                True
        )
      resExp = Ex $ TreeIR.ESEQ (initStm, rExp)
  in  (resExp, gen''')

relOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
relOp expLeft expRight op gen =
  let (expLeft' , gen' ) = unEx expLeft gen
      (expRight', gen'') = unEx expRight gen'
      treeOp             = transRelOp op
      resExp = Cx $ \t f -> TreeIR.CJUMP (treeOp, expLeft', expRight', t, f)
  in  (resExp, gen'')

transRelOp :: Absyn.Oper -> TreeIR.Relop
transRelOp op = case op of
  Absyn.EqOp  -> TreeIR.EQ
  Absyn.NeqOp -> TreeIR.NE
  Absyn.LtOp  -> TreeIR.LT
  Absyn.LeOp  -> TreeIR.LE
  Absyn.GtOp  -> TreeIR.GT
  Absyn.GeOp  -> TreeIR.GE
  _           -> error "shouldn't get here"

binOp :: Exp -> Exp -> Absyn.Oper -> Temp.Generator -> (Exp, Temp.Generator)
binOp expLeft expRight op gen =
  let (expLeft' , gen' ) = unEx expLeft gen
      (expRight', gen'') = unEx expRight gen'
      op'                = case op of
        Absyn.PlusOp   -> TreeIR.PLUS
        Absyn.MinusOp  -> TreeIR.MINUS
        Absyn.TimesOp  -> TreeIR.MUL
        Absyn.DivideOp -> TreeIR.DIV
        Absyn.ModOp    -> TreeIR.MOD
        _              -> error "shouldn't get here"
      binOpExp       = TreeIR.BINOP (op', expLeft', expRight')
      (resExp, gen3) = case op of
        Absyn.DivideOp -> case expRight' of
          TreeIR.CONST 0 ->
            error "shouldn't get here with (const) zero dividend"
          c@(TreeIR.CONST _) ->
            let opExp = TreeIR.BINOP (TreeIR.DIV, expLeft', c)
            in  (Ex opExp, gen'')
          _ ->
            let (testExp, gen''') = relOp expRight zeroExp Absyn.NeqOp gen''
                testExpGen        = unCx testExp
                (t, gen4)         = Temp.newlabel gen'''
                (f, gen5)         = Temp.newlabel gen4
                onDivByZero       = TreeIR.EXP $ X64Frame.externalCallNoReturn
                  (Temp.Label $ Symbol.Symbol "tiger_divByZero")
                  []
            in  ( Ex $ TreeIR.ESEQ
                  ( TreeIR.makeSeq
                    [ testExpGen t f
                    , TreeIR.LABEL (f, Nothing)
                    , onDivByZero
                    , TreeIR.LABEL (t, Nothing)
                    ]
                  , binOpExp
                  )
                , gen5
                )
        _ -> (Ex binOpExp, gen'')
  in  (resExp, gen3)

ifThen :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThen testExpE thenExpE gen =
  let
    cx                = unCx testExpE
    (thenStm, gen'  ) = unNx thenExpE gen
    (t      , gen'' ) = Temp.newlabel gen'
    (f      , gen''') = Temp.newlabel gen''
    resExp            = Nx $ TreeIR.makeSeq
      [cx t f, TreeIR.LABEL (t, Nothing), thenStm, TreeIR.LABEL (f, Nothing)]
  in
    (resExp, gen''')

seqExp :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
seqExp []    gen = (zeroExp, gen)
seqExp [exp] gen = (exp, gen)
seqExp (exp : exps) gen =
  let (headStm , gen'  ) = unNx exp gen
      (tailExp , gen'' ) = seqExp exps gen'
      (tailExp', gen''') = unEx tailExp gen''
      resExp             = Ex $ TreeIR.ESEQ (headStm, tailExp')
  in  (resExp, gen''')

seqStm :: [Exp] -> Temp.Generator -> (Exp, Temp.Generator)
seqStm [] gen = (zeroExp, gen)
seqStm (exp : exps) gen =
  let (headStm, gen'  ) = unNx exp gen
      (tailExp, gen'' ) = seqExp exps gen'
      (tailStm, gen''') = unNx tailExp gen''
      resStm            = Nx $ TreeIR.SEQ (headStm, tailStm)
  in  (resStm, gen''')

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
            IsStm -> Nx $ TreeIR.SEQ (headExpOrStm, tailExpOrStm)
            IsExp -> Ex $ TreeIR.ESEQ (headExpOrStm, tailExpOrStm)
  in
    (res, gen''')
-}

ifThenElseStm :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)

ifThenElseStm testExpE thenExp elseExp gen =
  let testGen           = unCx testExpE
      (t      , gen'  ) = Temp.newlabel gen
      (f      , gen'' ) = Temp.newlabel gen'
      (joinLab, gen''') = Temp.newlabel gen''
      (thenStm, gen4  ) = unNx thenExp gen'''
      (elseStm, gen5  ) = unNx elseExp gen4
      resExp            = Nx $ TreeIR.makeSeq
        [ testGen t f
        , TreeIR.LABEL (t, Nothing)
        , thenStm
        , TreeIR.JUMP (TreeIR.NAME joinLab, [joinLab])
        , TreeIR.LABEL (f, Nothing)
        , elseStm
        , TreeIR.LABEL (joinLab, Nothing)
        ]
  in  (resExp, gen5)

ifThenElse :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
ifThenElse (Cx testGen) (Ex (TreeIR.CONST 1)) (Cx elseGen) gen = -- orExp from Parser.y
  let (z, gen') = Temp.newlabel gen
      resExp    = Cx $ \t f -> TreeIR.SEQ
        (testGen t z, TreeIR.SEQ (TreeIR.LABEL (z, Nothing), elseGen t f))
  in  (resExp, gen')
ifThenElse (Cx testGen) (Cx thenGen) (Ex (TreeIR.CONST 0)) gen = -- andExp from Parser.y
  let (z, gen') = Temp.newlabel gen
      resExp    = Cx $ \t f -> TreeIR.SEQ
        (testGen z f, TreeIR.SEQ (TreeIR.LABEL (z, Nothing), thenGen t f))
  in  (resExp, gen')
ifThenElse testExpE thenExpE elseExpE gen =
  let testGen           = unCx testExpE
      (thenExp, gen'  ) = unEx thenExpE gen
      (elseExp, gen'' ) = unEx elseExpE gen'
      (t      , gen''') = Temp.newlabel gen''
      (f      , gen4  ) = Temp.newlabel gen'''
      (joinLab, gen5  ) = Temp.newlabel gen4
      (r      , gen6  ) = Temp.newtemp gen5
      resExp            = Ex $ TreeIR.ESEQ
        ( TreeIR.makeSeq
          [ testGen t f
          , TreeIR.LABEL (t, Nothing)
          , TreeIR.MOVE (TreeIR.TEMP r, thenExp)
          , TreeIR.JUMP (TreeIR.NAME joinLab, [joinLab])
          , TreeIR.LABEL (f, Nothing)
          , TreeIR.MOVE (TreeIR.TEMP r, elseExp)
          , TreeIR.LABEL (joinLab, Nothing)
          ]
        , TreeIR.TEMP r
        )
  in  (resExp, gen6)

assign :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
assign lhs rhs gen =
  let (lhsExp, gen' ) = unEx lhs gen
      (rhsExp, gen'') = unEx rhs gen'
      resExp          = Nx $ TreeIR.MOVE (lhsExp, rhsExp)
  in  (resExp, gen'')

assignConst :: Exp -> Int -> Temp.Generator -> (Exp, Temp.Generator)
assignConst lhs val gen =
  let (lhsExp, gen') = unEx lhs gen
      resExp         = Nx $ TreeIR.MOVE (lhsExp, TreeIR.CONST val)
  in  (resExp, gen')

letExpM :: [Exp] -> Exp -> State Temp.Generator Exp
letExpM initializers bodyExp = do
  gen <- get
  let (body           , gen' ) = unEx bodyExp gen
      (initializerStms, gen'') = runState (mapM unNxM initializers) gen'
  put gen''
  pure $ Ex $ TreeIR.ESEQ (TreeIR.makeSeq initializerStms, body)

letExp :: [Exp] -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
letExp initializers body = runState (letExpM initializers body)

while :: Exp -> Exp -> Temp.Label -> Temp.Generator -> (Exp, Temp.Generator)
while testExpE bodyExpE doneLab gen =
  let (testLab, gen'  ) = Temp.newlabel gen
      (bodyLab, gen'' ) = Temp.newlabel gen'
      (testExp, gen''') = unEx testExpE gen''
      (bodyExp, gen4  ) = unEx bodyExpE gen'''
      resExp            = Nx $ TreeIR.makeSeq
        [ TreeIR.LABEL (testLab, Nothing)
        , TreeIR.CJUMP (TreeIR.NE, zero, testExp, bodyLab, doneLab)
        , TreeIR.LABEL (bodyLab, Nothing)
        , TreeIR.EXP bodyExp
        , TreeIR.JUMP (TreeIR.NAME testLab, [testLab])
        , TreeIR.LABEL (doneLab, Nothing)
        ]
  in  (resExp, gen4)

break :: Temp.Label -> Exp
break breakTarget = Nx $ TreeIR.JUMP (TreeIR.NAME breakTarget, [breakTarget])

field :: Exp -> Int -> Temp.Generator -> (Exp, Temp.Generator)
field recordExpE fieldNumber gen =
  let (recordExp, gen') = unEx recordExpE gen
      happyPath         = Ex $ TreeIR.MEM $ TreeIR.BINOP
        (TreeIR.PLUS, recordExp, TreeIR.CONST $ fieldNumber * X64Frame.wordSize)
      sadPath = Ex $ X64Frame.externalCallNoReturn
        (Temp.Label $ Symbol.Symbol "tiger_nullRecordDereference")
        []
      jumpExp = Cx $ \happyLab sadLab ->
        TreeIR.CJUMP (TreeIR.NE, recordExp, zero, happyLab, sadLab)
  in  ifThenElse jumpExp happyPath sadPath gen'

setField :: Exp -> Int -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
setField recordExpE fieldNumber rhsExpE gen =
  let (recordExp, gen' ) = unEx recordExpE gen
      (rhsExp   , gen'') = unEx rhsExpE gen'
  in  (Nx $ setRecordField recordExp fieldNumber rhsExp, gen'')

setRecordField :: TreeIR.Exp -> Int -> TreeIR.Exp -> TreeIR.Stm
setRecordField recordExp fieldNumber rhsExp =
  let wordSize = X64Frame.wordSize
  in  TreeIR.MOVE
        ( TreeIR.MEM $ TreeIR.BINOP
          (TreeIR.PLUS, recordExp, TreeIR.CONST $ fieldNumber * wordSize)
        , rhsExp
        )

subscript :: Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
subscript arrExpE indexExpE gen =
  let (arrExp  , gen'  ) = unEx arrExpE gen
      (indexExp, gen'' ) = unEx indexExpE gen'
      (r       , gen''') = Temp.newtemp gen''
      rExp               = TreeIR.TEMP r
      resExp             = Ex $ TreeIR.ESEQ
        ( TreeIR.MOVE
          ( rExp
          , X64Frame.externalCall (Temp.Label $ Symbol.Symbol "tiger_getItem")
                                  [arrExp, indexExp]
                                  True
          )
        , rExp
        )
  in  (resExp, gen''')

setitem :: Exp -> Exp -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
setitem arrExpE subscriptE valE gen =
  let (arrExp      , gen'  ) = unEx arrExpE gen
      (subscriptExp, gen'' ) = unEx subscriptE gen'
      (valExp      , gen''') = unEx valE gen''
      (r           , gen4  ) = Temp.newtemp gen'''
      rExp                   = TreeIR.TEMP r
      resExp                 = Ex $ TreeIR.ESEQ
        ( TreeIR.MOVE
          ( rExp
          , X64Frame.externalCall (Temp.Label $ Symbol.Symbol "tiger_setItem")
                                  [arrExp, subscriptExp, valExp]
                                  True
          )
        , rExp
        )
  in  (resExp, gen4)


type Frag = X64Frame.Frag

string :: T.Text -> Temp.Generator -> (Exp, Frag, Temp.Generator)
string str gen =
  let (label, gen' ) = Temp.newlabel gen
      (r    , gen'') = Temp.newtemp gen'
      sExp           = TreeIR.TEMP r
      resExp         = Ex $ TreeIR.ESEQ
        ( TreeIR.MOVE
          ( sExp
          , X64Frame.externalCall
            (Temp.Label $ Symbol.Symbol "tiger_allocString")
            [TreeIR.NAME label, TreeIR.CONST $ T.length str]
            True
          )
        , sExp
        )
      frag = X64Frame.STRING (label, str)
  in  (resExp, frag, gen'')

unExM :: Exp -> State Temp.Generator TreeIR.Exp
unExM exp = do
  gen <- get
  let (treeExp, gen') = unEx exp gen
  put gen'
  pure treeExp

-- TODO: how to dry up unExM and unNxM? C++ templates (with duck-typing) could do it ...
unNxM :: Exp -> State Temp.Generator TreeIR.Stm
unNxM exp = do
  gen <- get
  let (treeStm, gen') = unNx exp gen
  put gen'
  pure treeStm

callM
  :: X64Level
  -> X64Level
  -> Temp.Label
  -> [Exp]
  -> Bool
  -> State Temp.Generator Exp
callM funLevel callerLevel funlab params hasRetVal = do
  gen <- get
  let (treeParams, gen') = runState (mapM unExM params) gen
      escapes            = x64Formals funLevel
      callerParentLevel  = x64Parent callerLevel
  put gen'
  pure $ Ex $ case x64Parent funLevel of
    X64Outermost -> X64Frame.externalCall funlab treeParams hasRetVal
    funParentLevel ->
      let sl = if funParentLevel == callerParentLevel
            then X64Frame.staticLinkExp $ x64Frame callerLevel
            else X64Frame.frameExp $ x64Frame callerLevel
      in  TreeIR.CALL (TreeIR.NAME funlab, sl : treeParams, escapes, hasRetVal)

call
  :: X64Level
  -> X64Level
  -> Temp.Label
  -> [Exp]
  -> Bool
  -> Temp.Generator
  -> (Exp, Temp.Generator)
call funLevel callerLevel funlab params hasRetVal =
  runState (callM funLevel callerLevel funlab params hasRetVal)

nilexp :: Exp
nilexp = Ex $ TreeIR.CONST 0

intexp :: Int -> Exp
intexp i = Ex $ TreeIR.CONST i

initExp
  :: X64Access -> X64Level -> Exp -> Temp.Generator -> (Exp, Temp.Generator)
initExp acc lev = assign $ simpleVar acc lev

functionDec :: X64Level -> Exp -> Temp.Generator -> (Frag, Temp.Generator)
functionDec X64Level { x64Frame = frame } bodyExp gen =
  let (bodyExpr , gen' ) = unEx bodyExp gen
      (bodyExpr', gen'') = X64Frame.procEntryExit1 frame bodyExpr gen'
      bodyStm            = TreeIR.MOVE (TreeIR.TEMP $ Frame.rv frame, bodyExpr')
  in  (makeProc bodyStm, gen'')
 where
  makeProc :: TreeIR.Stm -> Frag
  makeProc bodyStm =
    let resBody = TreeIR.SEQ
          ( TreeIR.LABEL (X64Frame.name frame, X64Frame.frameDebug frame)
          , bodyStm
          )
    in  X64Frame.PROC { X64Frame.body = resBody, X64Frame.fragFrame = frame }
functionDec X64Outermost _ _ = error "should not get here"

chaseStaticLinks :: X64Level -> X64Level -> TreeIR.Exp
chaseStaticLinks X64Outermost _ = error "outermost can't find static links"
chaseStaticLinks _ X64Outermost = error "can't find static links to outermost"
chaseStaticLinks callerLevel funParentLevel =
  let callerFrame = x64Frame callerLevel
      sl          = X64Frame.staticLinkAccess callerFrame
  in  if callerLevel == funParentLevel
        then X64Frame.frameExp callerFrame
        else X64Frame.exp sl
          $ chaseStaticLinks (x64Parent callerLevel) funParentLevel
