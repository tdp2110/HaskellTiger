module IROptim
  ( optimizeBasicBlock
  )
where

import qualified Canon
import qualified TreeIR                        as T

import qualified Data.Map                      as Map
import           Control.Monad.Trans.State      ( State
                                                , get
                                                , put
                                                , evalState
                                                )


optimizeBasicBlock :: Canon.Block -> Canon.Block
optimizeBasicBlock = propagateConstants

propagateConstants :: Canon.Block -> Canon.Block
propagateConstants bb = evalState (mapM propagateConstantsM bb) Map.empty
 where
  propagateConstantsM :: T.Stm -> State (Map.Map Int Int) T.Stm
  propagateConstantsM stm@(T.MOVE (T.TEMP dst, T.TEMP src)) = do
    constMap <- get
    case Map.lookup src constMap of
      Just constVal ->
        let constMap' = Map.insert dst constVal constMap
        in  do
              put constMap'
              pure $ T.MOVE (T.TEMP dst, T.CONST constVal)
      _ -> deleteFromConstMap stm
  propagateConstantsM stm@(T.MOVE (T.TEMP dst, T.CONST constVal)) = do
    constMap <- get
    let constMap' = Map.insert dst constVal constMap
    put constMap'
    pure stm
  propagateConstantsM stm@(T.MOVE (T.TEMP _, _)) = deleteFromConstMap stm
  propagateConstantsM stm                        = pure stm

  deleteFromConstMap :: T.Stm -> State (Map.Map Int Int) T.Stm
  deleteFromConstMap stm@(T.MOVE (T.TEMP dst, _)) = do
    constMap <- get
    let constMap' = Map.delete dst constMap
    put constMap'
    pure stm
  deleteFromConstMap stm = pure stm
