module FindEscape (escapeExp) where

import qualified Absyn as A
import Symbol

import Data.Map (Map)
import Data.List
import Prelude hiding (exp)


escapeExp :: A.Exp -> A.Exp

type AstPath = [AstDir]

-- a hand rolled zipper like thing. can this be generated??
data AstDir =
  CallArg Int
  | OpLeft
  | OpRight
  | RecField Int
  | SeqElt Int
  | AssignExp
  | IfTest
  | IfThen
  | IfElse
  | WhileTest
  | WhileBody
  | ForLo
  | ForHi
  | ForBody
  | LetDec Int
  | LetBody
  | ArraySize
  | ArrayInit
  | FunDec Int
  | FunParam Int
  deriving (Show)

data EnvEntry = EnvEntry{depth :: Int, path :: AstPath}
type Env = Map Symbol EnvEntry

findEscapes :: A.Exp -> [AstPath]
findEscapes _ = undefined

escapePaths :: A.Exp -> [AstPath] -> A.Exp
escapePaths exp paths =
  foldl' escapeExpPath exp paths

escapeExp exp = escapePaths exp (findEscapes exp)

escapeExpPath :: A.Exp -> AstPath -> A.Exp
escapeExpPath callExp@(A.CallExp _ args _) (CallArg(idx):path') =
  callExp{A.args=replaceNth idx args (escapeExpPath (args !! idx) path')}
escapeExpPath opExp@(A.OpExp leftExp _ _ _) (OpLeft:path') =
  opExp{A.left=escapeExpPath leftExp path'}
escapeExpPath opExp@(A.OpExp _ _ rightExp _) (OpRight:path') =
  opExp{A.right=escapeExpPath rightExp path'}
escapeExpPath recordExp@(A.RecordExp fields _ _) (RecField(idx):path') =
  let
    (sym, exp, pos) = fields !! idx
    exp' = escapeExpPath exp path'
  in
    recordExp{A.fields=replaceNth idx fields (sym, exp', pos)}
escapeExpPath (A.SeqExp seqElts) (SeqElt(idx):path') =
  let
    (exp, pos) = seqElts !! idx
    exp' = escapeExpPath exp path'
  in
    A.SeqExp $ replaceNth idx seqElts (exp', pos)
escapeExpPath assignExp@(A.AssignExp _ exp _) (AssignExp:path') =
  assignExp{A.exp=escapeExpPath exp path'}
escapeExpPath ifExp@(A.IfExp testExp _ _ _) (IfTest:path') =
  ifExp{A.test=escapeExpPath testExp path'}
escapeExpPath ifExp@(A.IfExp _ thenExp _ _) (IfThen:path') =
  ifExp{A.then'=escapeExpPath thenExp path'}
escapeExpPath ifExp@(A.IfExp _ _ (Just elseExp) _) (IfElse:path') =
  ifExp{A.else'=Just $ escapeExpPath elseExp path'}
escapeExpPath whileExp@(A.WhileExp testExp _ _) (WhileTest:path') =
  whileExp{A.test=escapeExpPath testExp path'}
escapeExpPath whileExp@(A.WhileExp _ bodyExp _) (WhileBody:path') =
  whileExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ _ _ _) [] = forExp{A.escape=True}
escapeExpPath forExp@(A.ForExp _ _ loExp _ _ _) (ForLo:path') =
  forExp{A.lo=escapeExpPath loExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ hiExp _ _) (ForHi:path') =
  forExp{A.hi=escapeExpPath hiExp path'}
escapeExpPath forExp@(A.ForExp _ _ _ _ bodyExp _) (ForBody:path') =
  forExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath letExp@(A.LetExp decs _ _) (LetDec(idx):path') =
  letExp{A.decs=replaceNth idx decs (escapeDecPath (decs !! idx) path')}
escapeExpPath letExp@(A.LetExp _ bodyExp _) (LetBody:path') =
  letExp{A.body=escapeExpPath bodyExp path'}
escapeExpPath arrayExp@(A.ArrayExp _ sizeExp _ _) (ArraySize:path') =
  arrayExp{A.size=escapeExpPath sizeExp path'}
escapeExpPath arrayExp@(A.ArrayExp _ _ initExp _) (ArrayInit:path') =
  arrayExp{A.init=escapeExpPath initExp path'}
escapeExpPath _ _ = error "shouldn't get here"

escapeDecPath :: A.Dec -> AstPath -> A.Dec
escapeDecPath (A.FunctionDec funDecs) [FunDec(funIdx), FunParam(paramIdx)] =
  A.FunctionDec $ replaceNth funIdx funDecs (escapeParam (funDecs !! funIdx) paramIdx)
  where
    escapeParam :: A.FunDec -> Int -> A.FunDec
    escapeParam funDec@(A.FunDec _ params _ _ _) idx =
      funDec{A.params=replaceNth idx params (escapeField $ params !! idx)}
    escapeField :: A.Field -> A.Field
    escapeField field@(A.Field _ _ _ _) = field{A.fieldEscape=True}
escapeDecPath varDec@(A.VarDec _ _ _ _ _) [] = varDec{A.vardecEscape=True}
escapeDecPath _ _ = error "shouldn't get here"

replaceNth :: Int -> [a] -> a -> [a]
replaceNth idx xs replacement =
  case splitAt idx xs of
    (xs', _:xs'') -> xs' ++ [replacement] ++ xs''
    _ -> error $ "invalid split index: " ++ (show idx)
