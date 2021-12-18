module Day17 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import qualified Data.Set as S

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

int :: GenParser Char st Int
int = (char '-' *> (((-1)*) <$> positiveInt)) <|> positiveInt

data TargetArea = TargetArea { 
  areaX1 :: Int, 
  areaX2 :: Int,
  areaY1 :: Int,
  areaY2 :: Int 
} deriving Show

parseTargetArea :: GenParser Char st TargetArea
parseTargetArea = do
  _ <- string "target area: x="
  x1 <- int
  _ <- string ".."
  x2 <- int
  _ <- string ", y="
  y1 <- int
  _ <- string ".."
  y2 <- int
  return (TargetArea x1 x2 y1 y2)

input :: IO (Either ParseError TargetArea)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day17.input"
  return $ parse parseTargetArea "day17.input" (T.unpack fileContent)

data Velocity = Velocity { vX :: Int, vY :: Int } deriving Show

data Position = Position { pX :: Int, pY :: Int } deriving (Eq, Ord, Show)

data Range = Range Double Double deriving Show

rangeAfterN :: Range -> Int -> Range
rangeAfterN (Range mn mx) n = let newMn = (mn * 2 / (fromIntegral n) + (fromIntegral n) - 1) / 2
                                  newMx = (mx * 2 / (fromIntegral n) + (fromIntegral n) - 1) / 2
                              in Range newMn newMx

yEnd :: Int -> Int -> Int
yEnd start n = (n * (2 * start + 1 - n)) `div` 2

findMxY :: Range -> (Range, Int)
findMxY r = last $ filter (\(Range mn mx, _) -> fromIntegral (floor mn) == mn || floor mn < floor mx) $ map (\i -> ((rangeAfterN r i), i)) [1..100000]

converged :: Range -> Range -> Bool
converged (Range x1 y1) (Range x2 y2) = abs (x1 - x2) < 0.5 && abs (y1 - y2) < 0.5

findConverged :: [Range] -> Maybe Range
findConverged [] = Nothing
findConverged [_] = Nothing
findConverged (r1 : tl@(r2 : _)) = if converged r1 r2 then Just r2 else findConverged tl

rangeForX :: Range -> Int -> Range
rangeForX r n = if n > 17 then rangeForX r 17
  else rangeAfterN r n

distinctVelocitiesForN :: Range -> Range -> Int -> [(Int, Int)]
distinctVelocitiesForN rX rY n =
  let (Range mnX mxX) = rangeForX rX n
      (Range mnY mxY) = rangeAfterN rY n
  in [(x, y) | x <- [ceiling mnX..floor mxX], y <- [ceiling mnY..floor mxY]]

distinctVelocities :: Range -> Range -> Int
distinctVelocities rX rY = S.size $ S.fromList $ concat $ map (distinctVelocitiesForN rX rY) [1..308]
