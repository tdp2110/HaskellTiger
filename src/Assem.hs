module Assem where

import qualified Temp

type Label = Temp.Label

data Inst = OPER { assem :: String
                 , operDst :: [Int]
                 , operSrc :: [Int]
                 , jump :: [Label] }
          | LABEL { assem :: String
                  , lab :: Label }
          | MOVE { assem :: String
                 , moveDst :: Int
                 , moveSrc :: Int }
  deriving (Eq, Show)
