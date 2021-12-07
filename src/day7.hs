module Day7 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.List as L

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

positions :: GenParser Char st [Int]
positions = sepBy1 positiveInt (char ',')

input :: IO (Either ParseError [Int])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day7.input"
  return $ parse positions "day7.input" (T.unpack fileContent)

type PositionCounts = M.Map Int Int

positionCounts :: [Int] -> PositionCounts
positionCounts = foldl' (\mp i -> M.insertWith (+) i 1 mp) M.empty . L.sort

cumulativeDistances :: Int -> Int -> PositionCounts -> [Int] -> [Int]
cumulativeDistances _ _ _ [] = []
cumulativeDistances cnt total pCounts (i : rest) = case M.lookup i pCounts of
  Nothing -> (total + cnt) : cumulativeDistances cnt (total + cnt) pCounts rest
  Just n -> (total + cnt) : cumulativeDistances (cnt + n) (total + cnt) pCounts rest

totalDistances :: [Int] -> [Int]
totalDistances is = let pCounts = positionCounts is
                        mn = minimum is
                        mx = maximum is
                        leftDistances = cumulativeDistances 0 0 pCounts [mn..mx]
                        rightDistances = reverse $ cumulativeDistances 0 0 pCounts [mx,mx-1..mn]
                    in map (uncurry (+)) (leftDistances `zip` rightDistances)

part1 :: IO (Either ParseError Int)
part1 = (fmap (minimum . totalDistances)) <$> input

totalAccumulatingDistances :: [Int] -> Int
totalAccumulatingDistances is = let pCounts = positionCounts is
                                    mn = minimum is
                                    mx = maximum is
                                    ld = cumulativeDistances 0 0 pCounts [mn..mx]
                                    rd = cumulativeDistances 0 0 pCounts [mx, mx-1..mn]
                                    leftDistances = scanl1 (+) ld
                                    rightDistances = reverse $ scanl1 (+) rd
                                in minimum $ map (uncurry (+)) $ zip leftDistances rightDistances

part2 :: IO (Either ParseError Int)
part2 = (fmap totalAccumulatingDistances) <$> input
