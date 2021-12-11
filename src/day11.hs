module Day10 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Char as C

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

type Indices = A.Array Int Int
type Grid = A.Array Int Int

indices :: Indices
indices = A.listArray (0, 99) [0..99]

parseLine :: GenParser Char st [Int]
parseLine = (map C.digitToInt) <$> many1 digit

grid :: GenParser Char st Grid
grid = do
  ls <- many1 (parseLine <* endOfLine)
  let joined = L.concat ls
  return (A.listArray (0, length joined - 1) joined)

input :: IO (Either ParseError Grid)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day11.input"
  return $ parse grid "day11.input" (T.unpack fileContent)

increaseEnergy :: Grid -> Grid
increaseEnergy = fmap (+1)

closeBy :: Int -> Int -> Bool
closeBy i j = let (r1, c1) = i `divMod` 10
                  (r2, c2) = j `divMod` 10
              in abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

neighbours :: Int -> [Int]
neighbours i = filter (\n -> n >= 0 && n < 100 && closeBy i n) 
  [i + 1, i - 1, i + 10, i - 10, i + 10 + 1, i + 10 - 1, i - 10 + 1, i - 10 - 1]

flash :: Grid -> Indices -> Grid
flash g is = fmap (\i -> let ns = neighbours i
                             increase = sum $ map (\n -> if g A.! n == 10 then 1 else 0) ns
                         in if g A.! i == 11 then 11
                            else if g A.! i == 10 
                            then 11 else (g A.! i + increase) `min` 10) is

hasTen :: Grid -> Bool
hasTen = foldr (\i b -> if i == 10 then True else b) False

resetFlashed :: Grid -> Grid
resetFlashed = fmap (\i -> if i >= 10 then 0 else i)

step :: Grid -> Grid
step gr = resetFlashed $ keepFlashing (increaseEnergy gr)
  where keepFlashing g = if hasTen g then keepFlashing (flash g indices) else g

countFlashes :: Grid -> Int
countFlashes = foldl' (\total i -> if i == 0 then total + 1 else total) 0

stepRepeatedly :: Int -> Int -> Grid -> Int
stepRepeatedly i flshs gr = if i == 0
  then flshs 
  else let next = step gr
           flashed = countFlashes next
       in stepRepeatedly (i - 1) (flshs  + flashed) next

part1 :: IO (Either ParseError Int)
part1 = (fmap (stepRepeatedly 100 0)) <$> input

allFlashed :: Grid -> Bool
allFlashed gr = countFlashes gr == 100

findStepWhereAllFlashed :: Grid -> Int
findStepWhereAllFlashed gr = find' 0 gr
  where find' i g = if allFlashed g then i else find' (i + 1) (step g)

part2 :: IO (Either ParseError Int)
part2 = (fmap findStepWhereAllFlashed) <$> input
