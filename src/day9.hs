module Day9 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Char as C

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Grid = Grid { rows :: Int, columns :: Int, entries :: A.Array Int Int } deriving Show

parseRow :: GenParser Char st [Int]
parseRow = (map C.digitToInt) <$> many1 digit

parseGrid :: GenParser Char st Grid
parseGrid = do
  rs <- many1 (parseRow <* endOfLine)
  let cs = length $ head rs
  let combined = L.concat rs
  let numRs = length rs
  return $ Grid numRs cs (A.listArray (0, (numRs * cs) - 1) combined)
  
input :: IO (Either ParseError Grid)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day9.input"
  return $ parse parseGrid "day9.input" (T.unpack fileContent)

closeBy :: Grid -> Int -> Int -> Bool
closeBy (Grid _ cs _) i j = let (r1, c1) = i `divMod` cs
                                (r2, c2) = j `divMod` cs
                                hor = r1 == r2 && (c1 == c2 + 1 || c1 == c2 - 1)
                                ver = c1 == c2 && (r1 == r2 + 1 || r1 == r2 - 1)
                            in hor || ver

neighbours :: Int -> Grid -> [Int]
neighbours i gr@(Grid rs cs _) = 
  filter (\j -> j >= 0 && j < rs * cs && closeBy gr i j) [i + 1, i - 1, i + cs, i - cs]

isLowPoint :: Grid -> Int -> Bool
isLowPoint gr@(Grid _ _ es) i = let ns = neighbours i gr
                                    hs = map (es A.!) ns
                                    ih = es A.! i
                                in null $ filter (<=ih) hs

findLowPoints :: Grid -> [Int]
findLowPoints gr@(Grid rs cs _) = filter (isLowPoint gr) [0..rs * cs - 1]

totalRiskLevel :: Grid -> Int
totalRiskLevel gr@(Grid _ _ es) = sum $ map ((+1) . (es A.!)) $ findLowPoints gr

part1 :: IO (Either ParseError Int)
part1 = (fmap totalRiskLevel) <$> input

type Components = M.Map Int Component
type Component = Int
type Sizes = M.Map Component Int

availableNeighbour :: Grid -> Components -> Int -> Bool
availableNeighbour (Grid _ _ es) comps i = not (M.member i comps) && (es A.! i) /= 9

exploreComponent :: Grid -> Component -> (Components, Sizes) -> Int -> (Components, Sizes)
exploreComponent gr c (comps, szs) i = 
  if M.member i comps 
  then (comps, szs)
  else let newComps = M.insert i c comps
           newSizes = M.insertWith (+) c 1 szs
           available = filter (availableNeighbour gr comps) $ neighbours i gr
       in foldl' (exploreComponent gr c)  (newComps, newSizes) available

connectedComponents :: Grid -> (Components, Sizes)
connectedComponents gr@(Grid rs cs es) = 
  fst $ foldl' (\st@(curr, c) i ->
            if (es A.! i == 9) || (M.member i (fst curr))
            then st 
            else let next = exploreComponent gr c curr i
                 in (next, c + 1)) ((M.empty, M.empty), 0) [0..(rs * cs - 1)]

largestBasinsProduct :: Grid -> Int
largestBasinsProduct gr = let (_, szs) = connectedComponents gr
                          in product $ take 3 $ L.sortOn (*(-1)) $ map snd $ M.toList szs

part2 :: IO (Either ParseError Int)
part2 = (fmap largestBasinsProduct) <$> input
