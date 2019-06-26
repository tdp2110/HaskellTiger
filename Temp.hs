module Temp where

import qualified Symbol


newtype Label = Label Symbol.Symbol

data Generator = Generator {tempIdx :: Int, labelIdx :: Int}

createGen :: Generator{tempInt=0, labelInt=0}

newtemp :: TempGen -> (idx, Generator)
newtemp gen@(Generator idx _) = (idx, gen{tempIdx=idx + 1})

newlabel ::TempGen -> (Label, Generator)
newlabel gen@(Generator _ idx) = (Label $ Symbol.Symbol $ "L" ++ show $ idx,
                                  gen{labelInt=idx + 1})
