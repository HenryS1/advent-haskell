module Day2 where

import Data.Foldable
import Text.Parsec.Char
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Instruction = Forward Int | Down Int | Up Int deriving Show

forward :: GenParser Char st Instruction
forward = Forward <$> ((string "forward ") *> positiveInt)

down :: GenParser Char st Instruction
down = Down <$> ((string "down ") *> positiveInt)

up :: GenParser Char st Instruction
up = Up <$> ((string "up ") *> positiveInt)

instruction :: GenParser Char st Instruction
instruction = forward <|> down <|> up

instructions :: GenParser Char st [Instruction]
instructions = many1 (instruction <* endOfLine)

input :: IO (Either ParseError [Instruction])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day2.input"
  return $ parse instructions "day2.input" (T.unpack fileContent)

data Position = Position { depth :: Int, distance :: Int } deriving Show

move :: Position -> Instruction -> Position
move p (Up i) = p { depth = depth p - i }
move p (Down i) = p { depth = depth p + i }
move p (Forward i) = p { distance = distance p + i }

navigate :: [Instruction] -> Position
navigate = foldl' move (Position 0 0)

part1 :: IO (Either ParseError Int)
part1 = (fmap (\p -> depth p * distance p)) <$> (fmap navigate) <$> input

data AimAndPosition = AimAndPosition { vertical :: Int, horizontal :: Int, aim :: Int }

moveWithAim :: AimAndPosition -> Instruction -> AimAndPosition
moveWithAim p (Up i) = p { aim = aim p - i }
moveWithAim p (Down i) = p { aim = aim p + i }
moveWithAim p (Forward i) = p { vertical = vertical p + i * aim p, horizontal = horizontal p + i }

navigateWithAim :: [Instruction] -> AimAndPosition
navigateWithAim = foldl' moveWithAim (AimAndPosition 0 0 0)

part2 :: IO (Either ParseError Int)
part2 = (fmap (\p -> vertical p * horizontal p)) <$> (fmap navigateWithAim) <$> input
