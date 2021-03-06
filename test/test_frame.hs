import           Test.Hspec

import qualified Temp
import qualified X64Frame

import qualified Data.Set                      as Set

main :: IO ()
main =
  hspec
    $ describe "AMD64"
    $ it "has disjoint callerSaves and calleeSaves lists"
    $ let gen         = Temp.newGen
          (amd64, _)  = X64Frame.initX64 gen
          calleeSaves = Set.fromList $ X64Frame.calleeSaves amd64
          callerSaves = Set.fromList $ X64Frame.callerSaves amd64
      in  (callerSaves `Set.disjoint` calleeSaves) `shouldBe` True
