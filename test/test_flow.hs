import Test.Hspec

import qualified Assem as A
import qualified Flow as F
import qualified Graph as G
import qualified Symbol as S
import qualified Temp

import qualified Data.Map as Map


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
        insts = [ A.OPER { A.assem="a <- 0"
                         , A.operDst=[a]
                         , A.operSrc=[]
                         , A.jump=Nothing }
                , A.LABEL { A.assem=".L1"
                          , A.lab=l2 }
                , A.OPER { A.assem="b <- a + 1"
                         , A.operDst=[b]
                         , A.operSrc=[a]
                         , A.jump=Nothing }
                , A.OPER { A.assem="c <- c + b"
                         , A.operDst=[c]
                         , A.operSrc=[c,b]
                         , A.jump=Nothing }
                , A.OPER { A.assem="a <- b * 2"
                         , A.operDst=[a]
                         , A.operSrc=[b]
                         , A.jump=Nothing }
                , A.OPER { A.assem="if a < N goto .L1"
                         , A.operDst=[]
                         , A.operSrc=[a]
                         , A.jump=Just [l1, l2] }
                , A.LABEL { A.assem="return c"
                          , A.lab=l1 }
                ]
        (g, nodes) = F.instrsToGraph insts
        defs = F.def g
        uses = F.use g
      in do
        length nodes `shouldBe` length insts
        length nodes `shouldBe` 7

        putStrLn $ G.toDot $ F.control g

        defs Map.! (F.NodeId 0) `shouldBe` [a]
        defs Map.! (F.NodeId 1) `shouldBe` []
        defs Map.! (F.NodeId 2) `shouldBe` [b]
        defs Map.! (F.NodeId 3) `shouldBe` [c]
        defs Map.! (F.NodeId 4) `shouldBe` [a]
        defs Map.! (F.NodeId 5) `shouldBe` []
        defs Map.! (F.NodeId 6) `shouldBe` []

        uses Map.! (F.NodeId 0) `shouldBe` []
        uses Map.! (F.NodeId 1) `shouldBe` []
        uses Map.! (F.NodeId 2) `shouldBe` [a]
        uses Map.! (F.NodeId 3) `shouldBe` [c,b]
        uses Map.! (F.NodeId 4) `shouldBe` [b]
        uses Map.! (F.NodeId 5) `shouldBe` [a]
        uses Map.! (F.NodeId 6) `shouldBe` []
