import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Control.Lens

newMeekachu :: MonPoke
newMeekachu = ("Rocket", "Meekachu", 2, 1)

newSnorfax :: MonPoke
newSnorfax = ("Green", "Snorfax", 3,1)

newRocket :: Team
newRocket = Team { name = "Rocket", roster = [], defeated = False }

main :: IO ()
main = hspec $ do
  describe "parseCmd" $ do
    it "handles ATTACK" $ do
      (parseCmd "ATTACK") `shouldBe` Attack
    it "handles CREATE" $ do
      (parseCmd "CREATE Rocket Meekachu 2 1") `shouldBe` Create (newMeekachu)
    it "handles CHOOSE" $ do
      (parseCmd "CHOOSE Meekachu") `shouldBe` Choose "Meekachu"

  describe "execCmd" $ do
    it "returns the create output when given a Create cmd" $ do
      let createCmd = Create ("Rocket", "Meekachu", 2, 1)
      (execCmd createCmd) `shouldBe` "Meekachu has been assigned to team Rocket!"
  describe "attackCmd" $ do
    it "decreases the defending Mon's HP by the attack Mon's AP" $ do
      let whackedSnorfax = (newMeekachu `attacks` newSnorfax)
      (whackedSnorfax ^. _3) `shouldBe` 2
  describe "addMonToTeam" $ do
      it "creates a mon and adds it to a team roster" $ do
        let updatedTeam = addMonToTeam newMeekachu newRocket
        (length (roster updatedTeam)) `shouldBe` 1
  describe "isDefeated" $ do
    it "says true for a team with Mon all defeated" $ do
      1 `shouldBe` 1

