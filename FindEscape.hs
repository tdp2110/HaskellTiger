module FindEscape where

import qualified Absyn as A
import Prelude hiding (exp)


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

escapeExp :: A.Exp -> AstPath -> A.Exp
escapeExp callExp@(A.CallExp _ args _) (CallArg(idx):path) =
  callExp{A.args=replaceNth idx args (escapeExp (args !! idx) path)}
escapeExp opExp@(A.OpExp leftExp _ _ _) (OpLeft:path) =
  opExp{A.left=escapeExp leftExp path}
escapeExp opExp@(A.OpExp _ _ rightExp _) (OpRight:path) =
  opExp{A.right=escapeExp rightExp path}
escapeExp recordExp@(A.RecordExp fields _ _) (RecField(idx):path) =
  let
    (sym, exp, pos) = fields !! idx
    exp' = escapeExp exp path
  in
    recordExp{A.fields=replaceNth idx fields (sym, exp', pos)}
escapeExp (A.SeqExp seqElts) (SeqElt(idx):path) =
  let
    (exp, pos) = seqElts !! idx
    exp' = escapeExp exp path
  in
    A.SeqExp $ replaceNth idx seqElts (exp', pos)
escapeExp assignExp@(A.AssignExp _ exp _) (AssignExp:path) =
  assignExp{A.exp=escapeExp exp path}
escapeExp ifExp@(A.IfExp testExp _ _ _) (IfTest:path) =
  ifExp{A.test=escapeExp testExp path}
escapeExp ifExp@(A.IfExp _ thenExp _ _) (IfThen:path) =
  ifExp{A.then'=escapeExp thenExp path}
escapeExp ifExp@(A.IfExp _ _ (Just elseExp) _) (IfElse:path) =
  ifExp{A.else'=Just $ escapeExp elseExp path}
escapeExp whileExp@(A.WhileExp testExp _ _) (WhileTest:path) =
  whileExp{A.test=escapeExp testExp path}
escapeExp whileExp@(A.WhileExp _ bodyExp _) (WhileBody:path) =
  whileExp{A.body=escapeExp bodyExp path}
escapeExp forExp@(A.ForExp _ _ _ _ _ _) [] = forExp{A.escape=True}
escapeExp forExp@(A.ForExp _ _ loExp _ _ _) (ForLo:path) =
  forExp{A.lo=escapeExp loExp path}
escapeExp forExp@(A.ForExp _ _ _ hiExp _ _) (ForHi:path) =
  forExp{A.hi=escapeExp hiExp path}
escapeExp forExp@(A.ForExp _ _ _ _ bodyExp _) (ForBody:path) =
  forExp{A.body=escapeExp bodyExp path}
escapeExp letExp@(A.LetExp decs _ _) (LetDec(idx):path) =
  letExp{A.decs=replaceNth idx decs (escapeDec (decs !! idx) path)}
escapeExp letExp@(A.LetExp _ bodyExp _) (LetBody:path) =
  letExp{A.body=escapeExp bodyExp path}
escapeExp arrayExp@(A.ArrayExp _ sizeExp _ _) (ArraySize:path) =
  arrayExp{A.size=escapeExp sizeExp path}
escapeExp arrayExp@(A.ArrayExp _ _ initExp _) (ArrayInit:path) =
  arrayExp{A.init=escapeExp initExp path}
escapeExp _ _ = error "shouldn't get here"

escapeDec :: A.Dec -> AstPath -> A.Dec
escapeDec (A.FunctionDec funDecs) [FunDec(funIdx), FunParam(paramIdx)] =
  A.FunctionDec $ replaceNth funIdx funDecs (escapeParam (funDecs !! funIdx) paramIdx)
  where

escapeDec varDec@(A.VarDec _ _ _ _ _) [] = varDec{A.vardecEscape=True}
escapeDec _ _ = error "shouldn't get here"

escapeParam :: A.FunDec -> Int -> A.FunDec
escapeParam funDec@(A.FunDec _ params _ _ _) idx =
  funDec{A.params=replaceNth idx params (escapeField $ params !! idx)}
  where
    escapeField :: A.Field -> A.Field
    escapeField field@(A.Field _ _ _ _) = field{A.fieldEscape=True}

replaceNth :: Int -> [a] -> a -> [a]
replaceNth idx xs replacement =
  case splitAt idx xs of
    (xs', _:xs'') -> xs' ++ [replacement] ++ xs''
    _ -> error $ "invalid split index: " ++ (show idx)
