module Day4 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M


positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)

data LineSegment = LineSegment { start :: Point, end :: Point } deriving (Eq, Ord, Show)

parsePoint :: GenParser Char st Point
parsePoint = do
  xCoord <- positiveInt
  _ <- char ','
  yCoord <- positiveInt
  return (Point xCoord yCoord)

parseLineSegment :: GenParser Char st LineSegment
parseLineSegment = do
  one <- parsePoint
  _ <- string " -> "
  other <- parsePoint
  return (LineSegment (one `min` other) (one `max` other))

lineSegments :: GenParser Char st [LineSegment]
lineSegments = many1 (parseLineSegment <* endOfLine)

input :: IO (Either ParseError [LineSegment])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day5.input"
  return $ parse lineSegments "day5.input" (T.unpack fileContent)

isVertical :: LineSegment -> Bool
isVertical (LineSegment (Point x1 _) (Point x2 _)) = x1 == x2 

verticalLines :: [LineSegment] -> [LineSegment]
verticalLines = filter isVertical

isHorizontal :: LineSegment -> Bool
isHorizontal (LineSegment (Point _ y1) (Point _ y2)) = y1 == y2

horizontalLines :: [LineSegment] -> [LineSegment]
horizontalLines = filter isHorizontal

horizontalCoverCount :: M.Map Point Int -> LineSegment ->  M.Map Point Int
horizontalCoverCount mp (LineSegment (Point x1 y1) (Point x2 _)) = 
  let coverRec :: Int -> M.Map Point Int -> M.Map Point Int
      coverRec currentX acc = if currentX > x2 
        then acc else coverRec (currentX + 1) (M.insertWith (+) (Point currentX y1) 1 acc)
  in coverRec x1 mp

allHorizontalCounts :: M.Map Point Int -> [LineSegment] -> M.Map Point Int
allHorizontalCounts = foldl' horizontalCoverCount

verticalCoverCount :: M.Map Point Int -> LineSegment -> M.Map Point Int
verticalCoverCount mp (LineSegment (Point x1 y1) (Point _ y2)) = 
  let coverRec :: Int -> M.Map Point Int -> M.Map Point Int
      coverRec currentY acc = if currentY > y2
        then acc else coverRec (currentY + 1) (M.insertWith (+) (Point x1 currentY) 1 acc)
  in coverRec y1 mp

allVerticalCounts :: M.Map Point Int -> [LineSegment] -> M.Map Point Int
allVerticalCounts = foldl' verticalCoverCount

coverCounts :: [LineSegment] -> M.Map Point Int
coverCounts ls = let vert = verticalLines ls
                     hor = horizontalLines ls
                 in allVerticalCounts (allHorizontalCounts M.empty hor) vert

coveredTwiceCount :: M.Map Point Int -> Int
coveredTwiceCount = foldl' (\total i -> if i > 1 then total + 1 else total) 0

part1 :: IO (Either ParseError Int)
part1 = (fmap (coveredTwiceCount . coverCounts)) <$> input

goingUpDiagonalCoverCount :: M.Map Point Int -> LineSegment -> M.Map Point Int
goingUpDiagonalCoverCount mp (LineSegment (Point x1 y1) (Point x2 _)) =
  let coverRec :: Int -> Int -> M.Map Point Int -> M.Map Point Int
      coverRec currentX currentY acc = if currentX > x2
        then acc else coverRec (currentX + 1) (currentY + 1) (M.insertWith (+) (Point currentX currentY) 1 acc)
  in coverRec x1 y1 mp

goingDownDiagonalCoverCount :: M.Map Point Int -> LineSegment -> M.Map Point Int
goingDownDiagonalCoverCount mp (LineSegment (Point x1 y1) (Point x2 _)) =
  let coverRec :: Int -> Int -> M.Map Point Int -> M.Map Point Int
      coverRec currentX currentY acc = if currentX > x2
        then acc else coverRec (currentX + 1) (currentY - 1) (M.insertWith (+) (Point currentX currentY) 1 acc)
  in coverRec x1 y1 mp

goingUpDiagonal :: LineSegment -> Bool
goingUpDiagonal (LineSegment (Point x1 y1) (Point x2 y2)) = x1 < x2 && y1 < y2

goingDownDiagonal :: LineSegment -> Bool
goingDownDiagonal (LineSegment (Point x1 y1) (Point x2 y2)) = x1 < x2 && y1 > y2

upDiagonalLines :: [LineSegment] -> [LineSegment]
upDiagonalLines = filter goingUpDiagonal

downDiagonalLines :: [LineSegment] -> [LineSegment]
downDiagonalLines = filter goingDownDiagonal

downDiagonalCounts :: M.Map Point Int -> [LineSegment] -> M.Map Point Int
downDiagonalCounts = foldl' goingDownDiagonalCoverCount

upDiagonalCounts :: M.Map Point Int -> [LineSegment] -> M.Map Point Int
upDiagonalCounts = foldl' goingUpDiagonalCoverCount

coverCountsWithDiagonal :: [LineSegment] -> M.Map Point Int
coverCountsWithDiagonal ls = let upDiag = upDiagonalLines ls
                                 downDiag = downDiagonalLines ls
                             in downDiagonalCounts (upDiagonalCounts (coverCounts ls) upDiag) downDiag

part2 :: IO (Either ParseError Int)
part2 = (fmap (coveredTwiceCount . coverCountsWithDiagonal)) <$> input
