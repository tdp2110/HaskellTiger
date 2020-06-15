import           Test.Hspec

import qualified Graph                         as G


newtype NodeId = NodeId Int

nodeId :: NodeId -> Int
nodeId (NodeId i) = i

instance Show NodeId where
  show (NodeId i) = "NodeId_" ++ show i

instance Eq NodeId where
  (NodeId n1) == (NodeId n2) = n1 == n2

instance Ord NodeId where
  (NodeId n1) `compare` (NodeId n2) = n1 `compare` n2

instance G.NodeId NodeId where
  incrId (NodeId nid) = NodeId $ nid + 1

main :: IO ()
main =
  hspec
    $ describe "toposort"
    $ it "works"
    $ let nid0            = NodeId 0
          g0              = G.newGraph nid0
          (node0, g1)     = G.newNode g0
          (node1, g2)     = G.newNode g1
          (node2, g3)     = G.newNode g2
          (node3, g4)     = G.newNode g3
          (node4, g5)     = G.newNode g4
          (node5, g6)     = G.newNode g5
          (node6, g7)     = G.newNode g6
          g8              = G.mkEdge g7 (G.nodeId node0) (G.nodeId node1)
          g9              = G.mkEdge g8 (G.nodeId node0) (G.nodeId node2)
          g10             = G.mkEdge g9 (G.nodeId node1) (G.nodeId node3)
          g11             = G.mkEdge g10 (G.nodeId node2) (G.nodeId node3)
          g12             = G.mkEdge g11 (G.nodeId node3) (G.nodeId node4)
          g13             = G.mkEdge g12 (G.nodeId node4) (G.nodeId node5)
          g14             = G.mkEdge g13 (G.nodeId node4) (G.nodeId node6)

          g               = g14

          topoSortedNodes = fmap (nodeId . G.nodeId) $ G.quasiTopoSort g
      in  do
            length (G.nodes g) `shouldBe` 7
            topoSortedNodes `shouldBe` [6, 5, 4, 3, 1, 2, 0]
