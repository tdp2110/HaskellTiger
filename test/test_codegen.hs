import           Test.Hspec

import qualified Assem
import qualified Codegen
import qualified Frame
import qualified Temp
import qualified TreeIR
import qualified X64Frame


main :: IO ()
main =
  hspec
    $ describe "regAlloc spill codegen"
    $ it "works"
    $ let
        gen             = Temp.newGen
        (x64   , gen' ) = X64Frame.initX64 gen
        (lab   , gen'') = Temp.newlabel gen'
        (gen''', frame) = X64Frame.newFrame
          x64
          lab
          Nothing
          gen''
          [Frame.Escapes, Frame.Escapes, Frame.DoesNotEscape]
        (gen4, frame', access) = X64Frame.allocLocal gen''' frame Frame.Escapes
        accessExp = X64Frame.exp access $ TreeIR.TEMP $ Frame.fp frame'
        (tempId, gen5)         = Temp.newtemp gen4
        storeStm               = TreeIR.MOVE (accessExp, TreeIR.TEMP tempId)
        (storeCode, gen6)      = Codegen.codegen x64 gen5 storeStm
        loadStm                = TreeIR.MOVE (TreeIR.TEMP tempId, accessExp)
        (loadCode, _)          = Codegen.codegen x64 gen6 loadStm
      in
        do
          length storeCode `shouldBe` 1
          length loadCode `shouldBe` 1
          let
            [Assem.OPER { Assem.operDst = [], Assem.operSrc = [storeDst, storeSrc] }]
              = storeCode
            [Assem.OPER { Assem.operDst = [loadDst], Assem.operSrc = [loadSrc] }]
              = loadCode
          storeDst `shouldBe` Frame.fp frame'
          storeSrc `shouldBe` tempId
          loadSrc `shouldBe` Frame.fp frame'
          loadDst `shouldBe` tempId
