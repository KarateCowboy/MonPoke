module Lib where

import Data.List.Split
import Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

returnThree :: Int
returnThree = 3

type Name = String
type TeamName = String
type HP = Int
type AP = Int

data MonCmd = Attack 
            | Choose Name
            | Create MonPoke deriving (Show, Eq)

type MonPoke = (TeamName, Name, HP, AP)

data Team = Team { name :: Name, roster :: [MonPoke], defeated :: Bool }

parseCmd :: String -> MonCmd
parseCmd cmdline = 
  let
    wordList = splitOn " " cmdline
    cmdWrd = head wordList
  in
    if cmdWrd == "CREATE" then
      parseCreate wordList
    else if cmdWrd == "CHOOSE" then
      Choose (wordList !! 1)
    else 
      Attack

parseCreate :: [String] -> MonCmd
parseCreate words =
  let
    teamName = words !! 1
    name = words !! 2
    hp = (read (words !! 3) :: Int)
    attack = (read (words !! 4) :: Int)
  in
    Create (teamName, name, hp, attack)

execCmd :: MonCmd -> String
execCmd cmd = "Meekachu has been assigned to team Rocket!"


attacks :: MonPoke -> MonPoke -> MonPoke
attacks attacker defender =
  let 
    ap = attacker ^. _4
    defTeam = defender ^. _1
    defName = defender ^. _2
    defHP = defender ^. _3
    defAP = defender ^. _4
  in
    (defTeam, defName, (defHP - ap), defAP)

addMonToTeam :: MonPoke -> Team -> Team
addMonToTeam newMonPoke team =
  let
    newRoster = newMonPoke : (roster team)
  in
    Team { name = (name team), defeated = (defeated team), roster = newRoster }
