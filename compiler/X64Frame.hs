{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module X64Frame
  ( Frag(..)
  , X64(..)
  , X64Frame(..)
  , X64Access(..)
  , Register(..)
  , MaxCallArgsAndEscapes(..)
  , X64Frame.exp
  , frameExp
  , externalCall
  , externalCallNoReturn
  , initX64
  , mainName
  , staticLinkAccess
  , staticLinkExp
  , allocLocal
  , newFrame
  , procEntryExit1
  , procEntryExit2
  , procEntryExit3
  , wordSize
  , registers
  )
where

import qualified Absyn
import qualified Assem
import qualified Frame
import qualified Symbol
import qualified Temp
import qualified TreeIR
import qualified Data.Text                     as T

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Prelude                 hiding ( exp )

data X64Access = InFrame Int | InReg Int
  deriving (Show)

mainName :: Symbol.Symbol
mainName = Symbol.Symbol "_main"

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
               , remainderRegister :: Int
               , multiplicandRegister :: Int
               , divideDests :: [Int]
               , multiplyDests :: [Int]
               , calleeSaves :: [Int]
               , callerSaves :: [Int]
               , callDests :: [Int]
               , paramRegs :: [Int]
               , tempMap :: Map Int Register }
  deriving (Show)

registers :: X64 -> [Register]
registers x64' = Map.elems $ tempMap x64'

data X64Frame = X64Frame { name :: Temp.Label
                         , formals :: [X64Access]
                         , locals :: [X64Access]
                         , x64 :: X64
                         , frameDebug :: Maybe (Symbol.Symbol, Absyn.Pos)
                         , viewShift :: [TreeIR.Stm] }

instance Show X64Frame where
  show f =
    "frame { name="
      ++ show (name f)
      ++ ", formals="
      ++ show (formals f)
      ++ ", locals="
      ++ show (locals f)
      ++ ", debug="
      ++ show (frameDebug f)
      ++ " }"

staticLinkAccess :: X64Frame -> X64Access
staticLinkAccess f = case formals f of
  []       -> error "expected static link in formals"
  (sl : _) -> sl

staticLinkExp :: X64Frame -> TreeIR.Exp
staticLinkExp f =
  let framePtr = frameExp f
  in  case staticLinkAccess f of
        (InReg _) -> error "static links should always be passed in memory"
        acc@(InFrame _) -> exp acc framePtr

data Frag = PROC { body :: TreeIR.Stm
                 , fragFrame :: X64Frame }
          | STRING (Temp.Label, T.Text)
  deriving (Show)

wordSize :: Int
wordSize = 8

exp :: X64Access -> TreeIR.Exp -> TreeIR.Exp
exp (InFrame k) framePtr =
  TreeIR.MEM $ TreeIR.BINOP (TreeIR.PLUS, framePtr, TreeIR.CONST $ k * wordSize)
exp (InReg regNum) _ = TreeIR.TEMP regNum

frameExp :: X64Frame -> TreeIR.Exp
frameExp frame = TreeIR.TEMP $ Frame.fp frame

externalCall :: Temp.Label -> [TreeIR.Exp] -> Bool -> TreeIR.Exp
externalCall (Temp.Label (Symbol.Symbol funname)) params hasRet = TreeIR.CALL
  ( TreeIR.NAME (Temp.Label (Symbol.Symbol $ T.cons '_' funname)) -- hack for MacOS
  , params
  , fmap (const Frame.DoesNotEscape) params
  , hasRet
  )

externalCallNoReturn :: Temp.Label -> [TreeIR.Exp] -> TreeIR.Exp
externalCallNoReturn (Temp.Label (Symbol.Symbol funname)) params =
  TreeIR.CALLNORETURN
    ( TreeIR.NAME (Temp.Label (Symbol.Symbol $ T.cons '_' funname)) -- hack for MacOS
    , params
    , fmap (const Frame.DoesNotEscape) params
    )

initX64 :: Temp.Generator -> (X64, Temp.Generator)
initX64 gen =
  let
    (raxId, gen' ) = Temp.newtemp gen
    (rbxId, gen'') = Temp.newtemp gen'
    (rcxId, gen3 ) = Temp.newtemp gen''
    (rdxId, gen4 ) = Temp.newtemp gen3
    (rbpId, gen5 ) = Temp.newtemp gen4
    (rsiId, gen6 ) = Temp.newtemp gen5
    (rdiId, gen7 ) = Temp.newtemp gen6
    (rspId, gen8 ) = Temp.newtemp gen7
    (r8Id , gen9 ) = Temp.newtemp gen8
    (r9Id , gen10) = Temp.newtemp gen9
    (r10Id, gen11) = Temp.newtemp gen10
    (r11Id, gen12) = Temp.newtemp gen11
    (r12Id, gen13) = Temp.newtemp gen12
    (r13Id, gen14) = Temp.newtemp gen13
    (r14Id, gen15) = Temp.newtemp gen14
    (r15Id, gen16) = Temp.newtemp gen15
  in
    ( X64
      { rax                  = raxId
      , rbx                  = rbxId
      , rcx                  = rcxId
      , rdx                  = rdxId
      , rbp                  = rbpId
      , rsi                  = rsiId
      , rdi                  = rdiId
      , rsp                  = rspId
      , r8                   = r8Id
      , r9                   = r9Id
      , r10                  = r10Id
      , r11                  = r11Id
      , r12                  = r12Id
      , r13                  = r13Id
      , r14                  = r14Id
      , r15                  = r15Id
      , dividendRegister     = raxId
      , remainderRegister    = rdxId
      , multiplicandRegister = raxId
      , divideDests          = [raxId, rdxId]
      , multiplyDests        = [raxId, rdxId]
      , calleeSaves          = [rbxId, rbpId, r12Id, r13Id, r14Id, r15Id]
      , callerSaves          = [ raxId
                               , rcxId
                               , rdxId
                               , rsiId
                               , rdiId
                               , r8Id
                               , r9Id
                               , r10Id
                               , r11Id
                               ]
      , callDests            = [raxId, rdxId]
      , paramRegs            = [rdiId, rsiId, rdxId, rcxId, r8Id, r9Id]
      , tempMap              = Map.fromList
                                 [ (raxId, RAX)
                                 , (rbxId, RBX)
                                 , (rcxId, RCX)
                                 , (rdxId, RDX)
                                 , (rbpId, RBP)
                                 , (rsiId, RSI)
                                 , (rdiId, RDI)
                                 , (rspId, RSP)
                                 , (r8Id , R8)
                                 , (r9Id , R9)
                                 , (r10Id, R10)
                                 , (r11Id, R11)
                                 , (r12Id, R12)
                                 , (r13Id, R13)
                                 , (r14Id, R14)
                                 , (r15Id, R15)
                                 ]
      }
    , gen16
    )

freshFrame :: Temp.Label -> X64 -> Maybe (Symbol.Symbol, Absyn.Pos) -> X64Frame
freshFrame frameName x64Inst maybeDebug = X64Frame { name       = frameName
                                                   , formals    = []
                                                   , locals     = []
                                                   , x64        = x64Inst
                                                   , frameDebug = maybeDebug
                                                   , viewShift  = []
                                                   }

data Register = RAX | RBX | RCX | RDX | RBP | RSI | RDI | RSP
              | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord)

instance Show Register where
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show RBP = "rbp"
  show RSI = "rsi"
  show RDI = "rdi"
  show RSP = "rsp"
  show R8  = "r8"
  show R9  = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"

instance Frame.Frame X64Frame where
  type Access X64Frame = X64Access
  type Arch X64Frame = X64
  type Register X64Frame = Register
  newFrame   = newFrame
  name       = name
  allocLocal = allocLocal
  formals    = formals
  locals     = locals
  rv frame = rax $ x64 frame
  fp frame = rbp $ x64 frame

newFrame
  :: X64
  -> Temp.Label
  -> Maybe (Symbol.Symbol, Absyn.Pos)
  -> Temp.Generator
  -> [Frame.EscapesOrNot]
  -> (Temp.Generator, X64Frame)
newFrame x64Inst frameName maybeDebug gen escapes =
  let initialFrame = freshFrame frameName x64Inst maybeDebug
      (gen', frame, _, _) =
          foldl' step (gen, initialFrame, 0, paramRegs x64Inst) escapes
  in  (gen', frame)
 where
  step
    :: (Temp.Generator, X64Frame, Int, [Int])
    -> Frame.EscapesOrNot
    -> (Temp.Generator, X64Frame, Int, [Int])
  step (gen', frame, numEscapesSeen, paramRegsRemaining) escapesOrNot =
    case escapesOrNot of
      Frame.DoesNotEscape -> case paramRegsRemaining of
        (paramReg : paramRegsRemaining') ->
          let (t, gen'') = Temp.newtemp gen'
              move       = TreeIR.MOVE (TreeIR.TEMP t, TreeIR.TEMP paramReg)
          in  ( gen''
              , frame { formals   = formals frame ++ [InReg t]
                      , viewShift = viewShift frame ++ [move]
                      }
              , numEscapesSeen
              , paramRegsRemaining'
              )
        [] ->
          step (gen', frame, numEscapesSeen, paramRegsRemaining) Frame.Escapes
      Frame.Escapes ->
        ( gen'  {- +2 to bypass pushed rbp on on function entry and implicitly-pushed rip from call instr. -}
        , frame { formals = formals frame ++ [InFrame $ numEscapesSeen + 2] }
        , numEscapesSeen + 1
        , paramRegsRemaining
        )

allocLocal
  :: Temp.Generator
  -> X64Frame
  -> Frame.EscapesOrNot
  -> (Temp.Generator, X64Frame, X64Access)
allocLocal gen frame escapesOrNot = case escapesOrNot of
  Frame.Escapes ->
    let numLocals = length $ filter isInFrame $ locals frame
        access    = InFrame $ -1 - numLocals
    in  (gen, frame { locals = locals frame ++ [access] }, access)
  Frame.DoesNotEscape ->
    let (regLabel, gen') = Temp.newtemp gen
        access           = InReg regLabel
    in  (gen', frame { locals = locals frame ++ [access] }, access)

isInFrame :: X64Access -> Bool
isInFrame (InFrame _) = True
isInFrame _           = False

isMain :: X64Frame -> Bool
isMain frame = name frame == Temp.Label mainName

-- | (From Appel, p 261) For each incoming register parameter, move it to the place
-- from which it is seen within the function (aka the "view shift"). This could be a frame location (for
-- escaping parameters) or a fresh temporary. One good way to handle this is for newFrame
-- to create a sequence of TreeIR.MOVE statements as it creates all the formal parameter
-- "accesses". newFrame can put this into the frame data structure, and procEntryExit1
-- can just concatenate it onto the procedue body.
--   Also concatenated to the body are statements for saving and restoring of callee-save registers.
-- procEntryExit1 should make up new temporaries for each callee-save (and return-address) register.
-- on entry, it should move all these registers to their new temporary locations,
-- and on exit, it should move them back. Of course, these moves (for nonspilled registers) will be
-- eliminated by register coalesching, so they cost nothing
procEntryExit1
  :: X64Frame -> TreeIR.Exp -> Temp.Generator -> (TreeIR.Exp, Temp.Generator)
procEntryExit1 frame bodyExp gen =
  let
    (t, gen')                = Temp.newtemp gen
    (saves, restores, gen'') = getSaveRestores gen'
    bodyTemp                 = TreeIR.TEMP t
  in
    if (not . isMain) frame
      then
        ( TreeIR.ESEQ
          ( TreeIR.makeSeq $ viewShift frame
          , TreeIR.ESEQ
            ( saves
            , TreeIR.ESEQ
              ( TreeIR.MOVE (bodyTemp, bodyExp)
              , TreeIR.ESEQ (restores, bodyTemp)
              )
            )
          )
        , gen''
        )
      else
        ( TreeIR.ESEQ
          ( TreeIR.makeSeq $ viewShift frame
          , TreeIR.ESEQ (TreeIR.MOVE (bodyTemp, bodyExp), bodyTemp)
          )
        , gen''
        )
 where
  getSaveRestores :: Temp.Generator -> (TreeIR.Stm, TreeIR.Stm, Temp.Generator)
  getSaveRestores g =
    let
      calleeSavesRegs        = calleeSaves (x64 frame) \\ [rbp $ x64 frame] -- rbp is pushed at function entry
      (calleeSavesTemps, g') = foldl' step ([], g) calleeSavesRegs
      step :: ([Int], Temp.Generator) -> Int -> ([Int], Temp.Generator)
      step (acc, gIn) _ =
        let (t, gOut) = Temp.newtemp gIn in (acc ++ [t], gOut)
      saves = fmap (\(t, r) -> TreeIR.MOVE (TreeIR.TEMP t, TreeIR.TEMP r))
                   (zip calleeSavesTemps calleeSavesRegs)
      restores =
        fmap (\(TreeIR.MOVE (dst, src)) -> TreeIR.MOVE (src, dst)) saves
    in
      (TreeIR.makeSeq saves, TreeIR.makeSeq restores, g')

-- | This function appends a "sink" instruction to the function body to tell the register allocator
-- that certain regisers are live at procedure exit.
-- fun procEntryExit2(frame, body) =
--   body @
--   [A.defaultOper{assem="",
--           src=[ZERO,RA,SP]@calleesaves,
--           dst=[],jump=SOME[]}]
procEntryExit2 :: X64Frame -> [Assem.Inst] -> [Assem.Inst]
procEntryExit2 frame bodyAsm =
  let x64'             = x64 frame
      maybeCalleeSaves = if isMain frame then [] else calleeSaves x64'
  in  bodyAsm
        ++ [ Assem.defaultOper
               { Assem.assem         = ""
               , Assem.operDst       = []
               , Assem.operSrc       = [rax x64', rsp x64', rbp x64']
                                         ++ maybeCalleeSaves
               , Assem.hasSideEffect = True
               , Assem.jump          = Just []
               }
           ]

newtype MaxCallArgsAndEscapes = MaxCallArgsAndEscapes (Maybe (Int, Int))

-- | Creates the procedure prologue and epilogue assembly language. Calculates the size of the
-- outgoing parameter space in the frame. This is roughly equal to the maximum number of outgoing parameters
-- of any CALL instruction in the procedure body. Once this is known, the assembly language
-- for procedure entry, stack pointer adjustment, and procedure exit can be put together;
-- these are the _prologue_ and _epilogue_.
procEntryExit3
  :: X64Frame -> [Assem.Inst] -> MaxCallArgsAndEscapes -> [Assem.Inst]
procEntryExit3 frame bodyAsm (MaxCallArgsAndEscapes maybeMaxCallArgsAndEscapes)
  = let
      (label : bodyAsm') = bodyAsm
      arch               = x64 frame
      stackSize =
        nextMultipleOf16 $ wordSize * case maybeMaxCallArgsAndEscapes of
          Just (maxCallArgs, maxEscapingParams) ->
            let stackSpaceForCallArgs =
                    max 0 (maxCallArgs - length (paramRegs arch)) --
                      + maxEscapingParams
            in  stackSpaceForCallArgs + inFrameCount
          Nothing -> -- we're in a leaf function (ie it calls no other function. use the 128-byte redzone)
            max 0 $ inFrameCount - 128 `div` wordSize
      stackAdjustment =
        [ Assem.defaultOper
            { Assem.assem   = T.pack $ "\tsub `d0, " ++ show stackSize
            , Assem.operDst = [rsp arch]
            , Assem.operSrc = []
            , Assem.jump    = Nothing
            }
        | stackSize /= 0
        ]
      prologue =
        [ Assem.defaultOper { Assem.assem   = "\tpush `d0"
                            , Assem.operDst = [rbp arch]
                            , Assem.operSrc = [rsp arch]
                            , Assem.jump    = Nothing
                            }
          , Assem.MOVE { Assem.assem   = "\tmov `d0, `s0"
                       , Assem.moveDst = rbp arch
                       , Assem.moveSrc = rsp arch
                       }
          ]
          ++ stackAdjustment
      epilogue1 =
        [ Assem.defaultOper
            { Assem.assem   = T.pack $ "\tadd `d0, " ++ show stackSize
            , Assem.operDst = [rsp arch]
            , Assem.operSrc = []
            , Assem.jump    = Nothing
            }
        | stackSize /= 0
        ]
      raxClearOrNil =
        [ Assem.defaultOper { Assem.assem   = "\txor `d0, `d0"
                            , Assem.operDst = [rax arch]
                            , Assem.operSrc = []
                            , Assem.jump    = Nothing
                            }
        | isMain frame
        ]
      epilogue2 =
        [ Assem.defaultOper { Assem.assem   = "\tpop `d0"
                            , Assem.operDst = [rbp arch]
                            , Assem.operSrc = []
                            , Assem.jump    = Nothing
                            }
          ]
          ++ raxClearOrNil
          ++ [ Assem.defaultOper { Assem.assem   = "\tret"
                                 , Assem.operDst = [rsp arch]
                                 , Assem.operSrc = []
                                 , Assem.jump    = Nothing
                                 }
             ]
      label'   = label { Assem.assem = T.pack $ T.unpack (Assem.assem label) }

      epilogue = epilogue1 ++ epilogue2
    in
      [label'] ++ prologue ++ bodyAsm' ++ epilogue
 where
  nextMultipleOf16 :: Int -> Int
  nextMultipleOf16 n = 16 * ((n + 15) `div` 16)

  inFrameCount :: Int
  inFrameCount = length $ fmap isInFrame $ locals frame
