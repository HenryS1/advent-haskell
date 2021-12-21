module Day21 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Player = Player { playerPid :: Int, playerPos :: Int, playerScore :: Int } deriving (Eq, Ord, Show)

parsePlayer :: GenParser Char st Player
parsePlayer = do
  _ <- string "Player "
  pid <- positiveInt
  _ <- string " starting position: "
  pos <- positiveInt
  return (Player pid pos 0)

data Game = Game Player Player deriving (Eq, Ord, Show)

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

type WinState = M.Map Int Integer

moveWithRoll :: Player -> Int -> Player
moveWithRoll (Player pid pos score) roll = 
  let finalPos = incMod pos roll
  in Player pid finalPos (score + finalPos)

type Cache = M.Map Game WinState

combineWins :: WinState -> WinState -> WinState
combineWins = M.unionWith (+)

possibleRolls :: [Int]
possibleRolls = map sum [[i, j, k] | i <- [1..3], j <- [1..3], k <- [1..3]]

numberOfWinsCached :: Int -> Cache -> Game -> (Cache, WinState)
numberOfWinsCached target cache gs@(Game p1@(Player pid1 _ _) p2@(Player pid2 _ _)) = 
  case M.lookup gs cache of
    Just wins -> (cache, wins)
    Nothing ->
      let playerWins :: Int -> Cache -> (Cache, WinState)
          playerWins i currCache = 
            let nextP1@(Player _ _ nextSc1) = moveWithRoll p1 i
            in if nextSc1 >= target 
               then let winState = (M.fromList [(pid1, 1), (pid2, 0)])
                    in (M.insertWith combineWins (Game p2 nextP1) winState currCache, winState)
               else numberOfWinsCached target currCache (Game p2 nextP1)
      in let (finalCache, finalWins) = foldl' (\(chc, wins) i -> 
                   let (newCache, newWins) = playerWins i chc
                   in (newCache, combineWins wins newWins)) (cache, M.empty) possibleRolls
         in (M.insert gs finalWins finalCache, finalWins)

answer :: Game -> (Cache, WinState)
answer gm = numberOfWinsCached 21 M.empty gm
                        
part2 :: IO (Either ParseError WinState)
part2 = (fmap (snd . answer)) <$> input 
