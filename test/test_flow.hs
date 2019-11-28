import Test.Hspec

import qualified Assem as A
import qualified Flow as F
import qualified Symbol as S
import qualified Temp

main :: IO ()
main = hspec $ do
  describe "CFG" $
    it "works" $
      let
        a = 1
        b = 2
        c = 3
        l1 = Temp.Label $ S.Symbol ".L1"
        l2 = Temp.Label $ S.Symbol ".L2"
        insts = [ A.OPER { A.assem=""
                         , A.operDst=[a]
                         , A.operSrc=[]
                         , A.jump=Nothing }
                , A.LABEL { A.assem=""
                          , A.lab=l2 }
                , A.OPER { A.assem=""
                         , A.operDst=[b]
                         , A.operSrc=[a]
                         , A.jump=Nothing }
                , A.OPER { A.assem=""
                         , A.operDst=[c]
                         , A.operSrc=[c,b]
                         , A.jump=Nothing }
                , A.OPER { A.assem=""
                         , A.operDst=[a]
                         , A.operSrc=[b]
                         , A.jump=Nothing }
                , A.OPER { A.assem=""
                         , A.operDst=[]
                         , A.operSrc=[a]
                         , A.jump=Just [l1, l2] }
                , A.LABEL { A.assem=""
                          , A.lab=l1 }
                ]
        (g, nodes) = F.instrsToGraph insts
      in do
        length nodes `shouldBe` length insts
        show g `shouldBe` "asdf"
