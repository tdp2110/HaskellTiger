{-# LANGUAGE TypeFamilies #-}

module X64Frame where

import qualified Absyn
import qualified Assem
import qualified Frame
import qualified Symbol
import qualified Temp
import qualified Tree

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (exp)


data X64Access = InFrame Int | InReg Int
  deriving (Show)

{-
We're implementing the AMD64 System V x86_64 calling convention.

On Linux and Mac (NOT windows) the first six integer or pointer arguments are passed in registers:
RDI, RSI, RDX, RCX, R8, R9. The rest are passed on the stack.

see https://en.wikipedia.org/wiki/X86_calling_conventions, "System V AMD64 ABI" subsection
or https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64
-}
data X64 = X64 { rax :: Int
               , rbx :: Int
               , rcx :: Int
               , rdx :: Int
               , rbp :: Int
               , rsi :: Int
               , rdi :: Int
               , rsp :: Int
               , r8 :: Int
               , r9 :: Int
               , r10 :: Int
               , r11 :: Int
               , r12 :: Int
               , r13 :: Int
               , r14 :: Int
               , r15 :: Int
               , dividendRegister :: Int
               , multiplicandRegister :: Int
               , divideDests :: [Int]
               , multiplyDests :: [Int]
               , calleeSaves :: [Int]
               , callerSaves :: [Int]
               , callDests :: [Int]
               , paramRegs :: [Int]
               , tempMap :: Map Int String }
  deriving (Show)

data X64Frame = X64Frame { name :: Temp.Label
                         , formals :: [X64Access]
                         , locals :: [X64Access]
                         , x64 :: X64
                         , frameDebug :: Maybe (Symbol.Symbol, Absyn.Pos)
                         , viewShift :: [Tree.Stm] }
  deriving (Show)

data Frag = PROC { body :: Tree.Stm
                 , fragFrame :: X64Frame }
          | STRING (Temp.Label, String)

wordSize :: Int
wordSize = 8

exp :: X64Access -> Tree.Exp -> Tree.Exp
exp (InFrame k) framePtr = Tree.MEM $ Tree.BINOP (Tree.PLUS, framePtr, Tree.CONST k)
exp (InReg regNum) _ = Tree.TEMP regNum

frameExp :: X64Frame -> Tree.Exp
frameExp frame = Tree.TEMP $ Frame.fp frame

externalCall :: Temp.Label -> [Tree.Exp] -> Tree.Exp
externalCall (Temp.Label (Symbol.Symbol funname)) params =
  Tree.CALL ( Tree.NAME (Temp.Label (Symbol.Symbol $ "_" ++ funname)) -- hack for MacOS
            , params
            , fmap (\_ -> Frame.NoEscape) params)

accessExp :: X64Frame -> X64Access -> Tree.Exp
accessExp frame acc = exp acc $ frameExp frame

staticLink :: Tree.Exp -> Tree.Exp
staticLink framePtr = Tree.MEM framePtr

numFormalsInReg :: X64Frame -> Int
numFormalsInReg frame =
  foldl' step (0 :: Int) (formals frame)
  where
    step :: Int -> X64Access -> Int
    step ct (InFrame _) = ct
    step ct (InReg _) = ct + 1

initX64 :: Temp.Generator -> (X64, Temp.Generator)
initX64 gen =
  let
    (raxId, gen') = Temp.newtemp gen
    (rbxId, gen'') = Temp.newtemp gen'
    (rcxId, gen3) = Temp.newtemp gen''
    (rdxId, gen4) = Temp.newtemp gen3
    (rbpId, gen5) = Temp.newtemp gen4
    (rsiId, gen6) = Temp.newtemp gen5
    (rdiId, gen7) = Temp.newtemp gen6
    (rspId, gen8) = Temp.newtemp gen7
    (r8Id, gen9) = Temp.newtemp gen8
    (r9Id, gen10) = Temp.newtemp gen9
    (r10Id, gen11) = Temp.newtemp gen10
    (r11Id, gen12) = Temp.newtemp gen11
    (r12Id, gen13) = Temp.newtemp gen12
    (r13Id, gen14) = Temp.newtemp gen13
    (r14Id, gen15) = Temp.newtemp gen14
    (r15Id, gen16) = Temp.newtemp gen15
  in
    ( X64 { rax=raxId
          , rbx=rbxId
          , rcx=rcxId
          , rdx=rdxId
          , rbp=rbpId
          , rsi=rsiId
          , rdi=rdiId
          , rsp=rspId
          , r8=r8Id
          , r9=r9Id
          , r10=r10Id
          , r11=r11Id
          , r12=r12Id
          , r13=r13Id
          , r14=r14Id
          , r15=r15Id
          , dividendRegister=raxId
          , multiplicandRegister=raxId
          , divideDests=[raxId, rdxId]
          , multiplyDests=[raxId, rdxId]
          , calleeSaves=[rbxId, rbpId, r12Id, r13Id, r14Id, r15Id]
          , callerSaves=[raxId, rcxId, rdxId, rsiId, rdiId, r8Id, r9Id, r10Id, r11Id]
          , callDests=[raxId, rdxId]
          , paramRegs=[rdiId, rsiId, rdxId, rcxId, r8Id, r9Id]
          , tempMap = Map.fromList [ (raxId, "rax")
                                   , (rbxId, "rbx")
                                   , (rcxId, "rcx")
                                   , (rdxId, "rdx")
                                   , (rbpId, "rbp")
                                   , (rsiId, "rsi")
                                   , (rdiId, "rdi")
                                   , (rspId, "rsp")
                                   , (r8Id,  "r8")
                                   , (r9Id,  "r9")
                                   , (r10Id, "r10")
                                   , (r11Id, "r11")
                                   , (r12Id, "r12")
                                   , (r13Id, "r13")
                                   , (r14Id, "r14")
                                   , (r15Id, "r15")
                                   ] }
    , gen16 )

freshFrame :: Temp.Label -> X64 -> Maybe (Symbol.Symbol, Absyn.Pos) -> X64Frame
freshFrame frameName x64Inst maybeDebug =
  X64Frame{ name=frameName
          , formals=[]
          , locals=[]
          , x64=x64Inst
          , frameDebug=maybeDebug
          , viewShift=[] }

instance Frame.Frame X64Frame where
  type (Access X64Frame) = X64Access
  type (Arch X64Frame) = X64
  newFrame = newFrame
  name = name
  allocLocal = allocLocal
  formals = formals
  locals = locals
  rv frame = rax $ x64 frame
  fp frame = rbp $ x64 frame

newFrame :: X64
            -> Temp.Label
            -> Maybe (Symbol.Symbol, Absyn.Pos)
            -> Temp.Generator
            -> [Frame.EscapesOrNot]
            -> (Temp.Generator, X64Frame)
newFrame x64Inst frameName maybeDebug gen escapes =
  let
    initialFrame = freshFrame frameName x64Inst maybeDebug
    (gen', frame, _, _) = foldl' step (gen, initialFrame, 0, paramRegs x64Inst) escapes
  in
    (gen', frame)
  where
    step :: (Temp.Generator, X64Frame, Int, [Int])
            -> Frame.EscapesOrNot
            -> (Temp.Generator, X64Frame, Int, [Int])
    step (gen', frame, numEscapesSeen, paramRegsRemaining) escapesOrNot =
      case escapesOrNot of
        Frame.NoEscape ->
          case paramRegsRemaining of
            (paramReg:paramRegsRemaining') ->
              let
                (t, gen'') = Temp.newtemp gen
                move = Tree.MOVE (Tree.TEMP t, Tree.TEMP paramReg)
              in
                ( gen''
                , frame{ formals=(formals frame) ++ [InReg t]
                       , viewShift=(viewShift frame) ++ [move] }
                , numEscapesSeen
                , paramRegsRemaining' )
            [] ->
              step (gen', frame, numEscapesSeen, paramRegsRemaining) Frame.Escapes
        Frame.Escapes ->
          ( gen'
          , frame{formals=(formals frame) ++ [InFrame numEscapesSeen]}
          , numEscapesSeen + 1
          , paramRegsRemaining )

allocLocal :: Temp.Generator -> X64Frame -> Frame.EscapesOrNot
  -> (Temp.Generator, X64Frame, X64Access)
allocLocal gen frame escapesOrNot =
  if Frame.escapes escapesOrNot then
    let
      numLocals = length $ filter isInFrame $ locals frame
      access = InFrame $ -1 - numLocals
    in
      (gen, frame{locals=(locals frame) ++ [access]}, access)
  else
    let
      (regLabel, gen') = Temp.newtemp gen
      access = InReg regLabel
    in
      (gen', frame{locals=(locals frame) ++ [access]}, access)
  where
    isInFrame :: X64Access -> Bool
    isInFrame (InFrame _) = True
    isInFrame _ = False

-- | (From Appel, p 261) For each incoming register parameter, move it to the place
-- from which it is seen within the function (aka the "view shift"). This could be a frame location (for
-- escaping parameters) or a fresh temporary. One good way to handle this is for newFrame
-- to create a sequence of Tree.MOVE statements as it creates all the formal parameter
-- "accesses". newFrame can put this into the frame data structure, and procEntryExit1
-- can just concatenate it onto the procedue body.
--   Also concatenated to the body are statements for saving and restoring of callee-save registers.
-- procEntryExit1 should make up new temporaries for each callee-save (and return-address) register.
-- on entry, it should move all these registers to their new temporary locations,
-- and on exit, it should move them back. Of course, these moves (for nonspilled registers) will be
-- eliminated by register coalesching, so they cost nothing
procEntryExit1 :: X64Frame -> Tree.Exp -> Temp.Generator -> (Tree.Exp, Temp.Generator)
procEntryExit1 frame bodyExp gen =
  let
    (t, gen') = Temp.newtemp gen
    (saves, restores, gen'') = getSaveRestores gen'
    bodyTemp = Tree.TEMP t
  in
    ( Tree.ESEQ ( Tree.makeSeq $ viewShift frame
                , Tree.ESEQ ( saves
                            , Tree.ESEQ ( Tree.MOVE (bodyTemp, bodyExp)
                                        , Tree.ESEQ (restores, bodyTemp))))
    , gen'')
  where
    getSaveRestores :: Temp.Generator -> (Tree.Stm, Tree.Stm, Temp.Generator)
    getSaveRestores g =
      let
        calleeSavesRegs = calleeSaves $ x64 frame
        (calleeSavesTemps, g') = foldl'
                                   step
                                   ([], g)
                                   calleeSavesRegs
        step :: ([Int], Temp.Generator) -> Int -> ([Int], Temp.Generator)
        step (acc, gIn) _ =
          let
            (t, gOut) = Temp.newtemp gIn
          in
            (acc ++ [t], gOut)
        saves = fmap
                  (\(t, r) -> Tree.MOVE (Tree.TEMP t, Tree.TEMP r))
                  (zip calleeSavesTemps calleeSavesRegs)
        restores = fmap
                     (\(Tree.MOVE (dst, src)) -> Tree.MOVE (src, dst))
                     saves

      in
        (Tree.makeSeq saves, Tree.makeSeq restores, g')

-- | This function appends a "sink" instruction to the function body to tell the register allocator
-- that certain regisers are live at procedure exit. In the case of the Jouette machine, this is simply
-- fun procEntryExit2(frame, body) =
--   body @
--   [A.OPER{assem="",
--           src=[ZERO,RA,SP]@calleesaves,
--           dst=[],jump=SOME[]}]
procEntryExit2 :: X64Frame -> [Assem.Inst] -> [Assem.Inst]
procEntryExit2 frame bodyAsm =
  let
    x64' = x64 frame
  in
    bodyAsm ++ [
        Assem.OPER { Assem.assem = ""
                   , Assem.operDst = []
                   , Assem.operSrc = [rax x64', rsp x64'] ++ calleeSaves x64'
                   , Assem.jump = Just [] }
            ]

-- | Creates the procedure prologue and epilogue assembly language. Calculates the size of the
-- outgoing parameter space in the frame. This is equal to the maximum number of outgoing parameters
-- of any CALL instruction in the procedure body. Once this is known, the assembly language
-- for procedure entry, stack pointer adjustment, and procedure exit can be put together;
-- these are the _prologue_ and _epilogue_.
procEntryExit3 :: X64Frame -> [Assem.Inst] -> [Assem.Inst]
procEntryExit3 frame bodyAsm =
  let
    (label:bodyAsm') = bodyAsm
    stackSize = 1024 :: Int -- TODO! set up with real value! need to know number of spilled locals
                            -- and max number of outgoing params
    prologue = [ Assem.OPER { Assem.assem="\tpush `d0" ++ (fmtDebug frame)  ++ "\n"
                            , Assem.operDst=[rsp $ x64 frame]
                            , Assem.operSrc=[rbp $ x64 frame]
                            , Assem.jump=Nothing }
               , Assem.MOVE { Assem.assem="\tmov `d0, `s0\n"
                            , Assem.moveDst=rbp $ x64 frame
                            , Assem.moveSrc=rsp $ x64 frame }
               , Assem.OPER { Assem.assem="\tsub `d0, " ++ (show stackSize) ++ "\n"
                            , Assem.operDst=[rsp $ x64 frame]
                            , Assem.operSrc=[]
                            , Assem.jump=Nothing } ]
    epilogue = [ Assem.OPER { Assem.assem="\tadd `d0, " ++ (show stackSize) ++ "\n"
                            , Assem.operDst=[rsp $ x64 frame]
                            , Assem.operSrc=[]
                            , Assem.jump=Nothing }
               , Assem.OPER { Assem.assem="\tpop `d0\n"
                            , Assem.operDst=[rbp $ x64 frame]
                            , Assem.operSrc=[]
                            , Assem.jump=Nothing }
               , Assem.OPER { Assem.assem="\tret\n"
                            , Assem.operDst=[rsp $ x64 frame]
                            , Assem.operSrc=[]
                            , Assem.jump=Nothing } ]
  in
    [label] ++ prologue ++ bodyAsm' ++ epilogue
  where
    fmtDebug :: X64Frame -> String
    fmtDebug (X64Frame { frameDebug=Just dbg }) = "\t\t ; " ++ show dbg
    fmtDebug _ = ""