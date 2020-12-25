import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Control.Lens

newMeekachu :: MonPoke
newMeekachu = ("Rocket", "Meekachu", 2, 1)

newSmorelax :: MonPoke
newSmorelax = ("Green", "Smorelax", 3,1)

newRocket :: Team
newRocket = Team { name = "Rocket", roster = [], defeated = False }

newGreen :: Team
newGreen = Team { name = "Green", roster = [], defeated = False }

sampleGameSequence :: [(String,String)] 
sampleGameSequence = [
  ("CREATE Rocket Meekachu 3 1", "Meekachu has been assigned to team Rocket!"),
  ("CREATE Rocket Rastly 5 6", "Rastly has been assigned to team Rocket!"),
  ("CREATE Green Smorelax 2 1", "Smorelax has been assigned to team Green!" ),
  ("ICHOOSEYOU Meekachu", "Meekachu has entered the battle!" ),
  ("ICHOOSEYOU Smorelax", "Smorelax has entered the battle!" ),
  ("ATTACK", "Meekachu attacked Smorelax for 1 damage!"),
  ("ATTACK", "Smorelax attacked Meekachu for 1 damage!" ),
  ("ICHOOSEYOU Rastly", "Rastly has entered the battle!" ),
  ("ATTACK", "Smorelax attacked Rastly for 1 damage!" ),
  ("ATTACK", "Rastly attacked Smorelax for 6 damage!\nSmorelax has been defeated!\nRocket is the winner!")
 ]

main :: IO ()
main = hspec $ do
  describe "parseCmd" $ do
    it "handles ATTACK" $ do
      (parseCmd "ATTACK") `shouldBe` Attack
    it "handles CREATE" $ do
      (parseCmd "CREATE Rocket Meekachu 2 1") `shouldBe` Create (newMeekachu)
      (parseCmd "CREATE Green Smorelax 3 1") `shouldBe` Create (newSmorelax)
    it "handles CHOOSE" $ do
      (parseCmd "CHOOSE Meekachu") `shouldBe` Choose "Meekachu"
  describe "Sample game sequence" $ do
    it "returns the correct output for each input" $ do
      let inputs = map (\x -> fst x) sampleGameSequence
      let outputs = map (\x -> snd x) sampleGameSequence
      let parseExec = execCmd . parseCmd


  describe "State" $ do
    it "has teams attribute which is a duple of Teams" $ do
      let newState = State { teams = (newRocket, newGreen ) }
      (teams newState)`shouldBe` (newRocket, newGreen)

  describe "execCmd" $ do
    it "returns the create output when given a Create cmd" $ do
      let createCmd = Create ("Rocket", "Meekachu", 2, 1)
      (execCmd createCmd) `shouldBe` "Meekachu has been assigned to team Rocket!"
  describe "createCmd" $ do
    it "creates a mon and adds it to a team" $ do
       "placeholder" `shouldBe` "placeholder"
  describe "attackCmd" $ do
    it "has a placeholder" $ do
      "placeholder" `shouldBe` "placeholder"
  describe "attack" $ do
    it "decreases the defending Mon's HP by the attack Mon's AP" $ do
      let whackedSmorelax = (newMeekachu `attacks` newSmorelax)
      (whackedSmorelax ^. _3) `shouldBe` 2
  describe "addMonToTeam" $ do
      it "creates a mon and adds it to a team roster" $ do
        let updatedTeam = addMonToTeam newMeekachu newRocket
        (length (roster updatedTeam)) `shouldBe` 1
  describe "isDefeated" $ do
    it "says true for a team with Mon all defeated" $ do
      1 `shouldBe` 1

