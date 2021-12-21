module Day21 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Array as A
import qualified Data.Map as M

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Player = Player { playerPid :: Int, playerPos :: Int, playerScore :: Int } deriving Show

parsePlayer :: GenParser Char st Player
parsePlayer = do
  _ <- string "Player "
  pid <- positiveInt
  _ <- string " starting position: "
  pos <- positiveInt
  return (Player pid pos 0)

data Game = Game Player Player deriving Show

parseGame :: GenParser Char st Game
parseGame = do
  p1 <- parsePlayer 
  _ <- endOfLine
  p2 <- parsePlayer
  _ <- endOfLine
  return (Game p1 p2)

input :: IO (Either ParseError Game)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day21.input"
  return $ parse parseGame "day21.input" (T.unpack fileContent)

incMod :: Int -> Int -> Int
incMod pos roll = ((pos + roll - 1) `mod` 10) + 1

movePlayer :: Player -> [Int] -> (Player, [Int])
movePlayer (Player pid pos score) d = 
  let (taken, newD) = splitAt 3 d
      roll = sum taken
      finalPos = incMod pos roll 
  in (Player pid finalPos (score + finalPos), newD)

play :: [Int] -> Game -> (Game, Int)
play d (Game p1 p2) = 
  let (newP1@(Player _ _ p1Score), nextD) = movePlayer p1 d
  in if p1Score >= 1000 then (Game newP1 p2, head nextD)
     else let (newP2@(Player _ _ p2Score), finalD) = movePlayer p2 nextD
          in if p2Score >= 1000 then (Game newP1 newP2, head finalD)
             else play finalD (Game newP1 newP2) 

part1 :: IO (Either ParseError (Game, Int))
part1 = (fmap (play [1..])) <$> input

type Target = Int

type WinState = M.Map Int Int

moveWithRoll :: Player -> Int -> Player
moveWithRoll (Player pid pos score) roll = 
  let finalPos = incMod pos roll
  in Player pid finalPos (score + finalPos)

data Turn = P1 | P2 deriving Show

numberOfWins :: Player -> Player -> Target -> WinState
numberOfWins p1@(Player pid1 _ _) (Player pid2 pos2 sc2) target = 
  let playerWins i = let (Player _ nextPos1 nextSc1) = moveWithRoll p1 i
                     in if nextSc1 > target then (M.fromList [(pid1, 1), (pid2, 0)])
                        else let minScore = nextSc1 `min` sc2
                                 finalP1 = Player pid1 nextPos1 (nextSc1 - minScore)
                                 finalP2 = Player pid2 pos2 (sc2 - minScore)
                                 nextTarget = target - minScore
                             in numberOfWins finalP2 finalP1 nextTarget
      wins1 = playerWins 1
      wins2 = playerWins 2
      wins3 = playerWins 3
  in M.unionWith (+) (M.unionWith (+) wins1 wins2) wins3
