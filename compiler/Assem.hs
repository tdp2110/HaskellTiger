{-# LANGUAGE OverloadedStrings #-}

module Assem
  ( Inst(..)
  , Label
  , defaultOper
  )
where

import qualified Temp
import qualified Data.Text                     as T

type Label = Temp.Label

data Inst = OPER { assem :: T.Text
                 , operDst :: [Int]
                 , operSrc :: [Int]
                 , hasFallthrough :: Bool
                 , hasSideEffect :: Bool
                 , calls :: Maybe Label
                 , noreturn :: Bool
                 , jump :: Maybe [Label] }
          | LABEL { assem :: T.Text
                  , lab :: Label }
          | MOVE { assem :: T.Text
                 , moveDst :: Int
                 , moveSrc :: Int }
          | STORECONST { assem :: T.Text
                       , strDst :: Int
                       , strVal :: Int }
  deriving (Eq, Show)

defaultOper :: Inst
defaultOper = OPER { assem          = ""
                   , operDst        = []
                   , operSrc        = []
                   , hasSideEffect  = False
                   , hasFallthrough = False
                   , calls          = Nothing
                   , noreturn       = False
                   , jump           = Nothing
                   }
