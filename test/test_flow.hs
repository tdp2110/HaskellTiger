{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified Assem                         as A
import qualified Flow                          as F
import qualified Graph                         as G
import qualified Liveness                      as L
import qualified Symbol                        as S
import qualified Temp

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T

main :: IO ()
main =
  hspec
    $ describe "CFG and IGraph"
    $ it "works"
    $ let a  = 1
          b  = 2
          c  = 3
          l1 = Temp.Label $ S.Symbol ".L1"
          l2 = Temp.Label $ S.Symbol ".L2"
          insts =
            [ A.OPER { A.assem   = "a <- 0"
                     , A.operDst = [a]
                     , A.operSrc = []
                     , A.jump    = Nothing
                     }
            , A.LABEL { A.assem = ".L1", A.lab = l2 }
            , A.OPER { A.assem   = T.pack "b <- a + 1"
                     , A.operDst = [b]
                     , A.operSrc = [a]
                     , A.jump    = Nothing
                     }
            , A.OPER { A.assem   = "c <- c + b"
                     , A.operDst = [c]
                     , A.operSrc = [c, b]
                     , A.jump    = Nothing
                     }
            , A.OPER { A.assem   = "a <- b * 2"
                     , A.operDst = [a]
                     , A.operSrc = [b]
                     , A.jump    = Nothing
                     }
            , A.OPER { A.assem   = "if a < N goto .L1"
                     , A.operDst = []
                     , A.operSrc = [a]
                     , A.jump    = Just [l1, l2]
                     }
            , A.LABEL { A.assem = "return c", A.lab = l1 }
            ]
          (flowGraph, nodes) = F.instrsToGraph insts
          defs               = F.def flowGraph
          uses               = F.use flowGraph
          (igraph, liveMap)  = L.interferenceGraph flowGraph
          tnode              = L.tnode igraph
          gtemp              = L.gtemp igraph
          igraphGraph        = L.graph igraph
      in  do
            length nodes `shouldBe` length insts
            length nodes `shouldBe` 7

            -- putStrLn "ControlFlowGraph:"
            -- putStrLn $ G.toDot $ F.control flowGraph

            defs Map.! F.NodeId 0 `shouldBe` [a]
            defs Map.! F.NodeId 1 `shouldBe` []
            defs Map.! F.NodeId 2 `shouldBe` [b]
            defs Map.! F.NodeId 3 `shouldBe` [c]
            defs Map.! F.NodeId 4 `shouldBe` [a]
            defs Map.! F.NodeId 5 `shouldBe` []
            defs Map.! F.NodeId 6 `shouldBe` []

            uses Map.! F.NodeId 0 `shouldBe` []
            uses Map.! F.NodeId 1 `shouldBe` []
            uses Map.! F.NodeId 2 `shouldBe` [a]
            uses Map.! F.NodeId 3 `shouldBe` [c, b]
            uses Map.! F.NodeId 4 `shouldBe` [b]
            uses Map.! F.NodeId 5 `shouldBe` [a]
            uses Map.! F.NodeId 6 `shouldBe` []

            -- putStrLn "InterferenceGraph:"
            -- putStrLn $ G.toDot igraphGraph

            Map.size liveMap `shouldBe` length nodes

            liveMap Map.! F.NodeId 0 `shouldBe` Set.fromList [1, 3]
            liveMap Map.! F.NodeId 1 `shouldBe` Set.fromList [1, 3]
            liveMap Map.! F.NodeId 2 `shouldBe` Set.fromList [2, 3]
            liveMap Map.! F.NodeId 3 `shouldBe` Set.fromList [2, 3]
            liveMap Map.! F.NodeId 4 `shouldBe` Set.fromList [1, 3]
            liveMap Map.! F.NodeId 5 `shouldBe` Set.fromList [1, 3]
            liveMap Map.! F.NodeId 6 `shouldBe` Set.fromList []

            G.nodeId (tnode Map.! a) `shouldBe` L.NodeId 0
            G.nodeId (tnode Map.! b) `shouldBe` L.NodeId 1
            G.nodeId (tnode Map.! c) `shouldBe` L.NodeId 2

            gtemp Map.! L.NodeId 0 `shouldBe` a
            gtemp Map.! L.NodeId 1 `shouldBe` b
            gtemp Map.! L.NodeId 2 `shouldBe` c

            G.hasEdge igraphGraph (L.NodeId 0) (L.NodeId 2) `shouldBe` True
            G.hasEdge igraphGraph (L.NodeId 2) (L.NodeId 0) `shouldBe` False
            G.hasEdge igraphGraph (L.NodeId 1) (L.NodeId 2) `shouldBe` True
            G.hasEdge igraphGraph (L.NodeId 2) (L.NodeId 1) `shouldBe` True

            length (G.edges igraphGraph) `shouldBe` 3
