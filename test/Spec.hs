import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "thing" $ do
    it "works" $ do
      3 `shouldBe` 3


