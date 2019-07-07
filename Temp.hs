module Temp where

import qualified Symbol


newtype Label = Label Symbol.Symbol

data Generator = Generator {tempIdx :: Int, labelIdx :: Int}

newGen :: Generator
newGen = Generator{tempIdx=0, labelIdx=0}

newtemp :: Generator -> (Int, Generator)
newtemp gen@(Generator idx _) = (idx, gen{tempIdx=idx + 1})

newlabel :: Generator -> (Label, Generator)
newlabel gen@(Generator _ idx) = (Label $ Symbol.Symbol $ "L" ++ show idx,
                                  gen{labelIdx=idx + 1})
