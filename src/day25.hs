module Day25 where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Array.Unboxed as A
import Data.List (intercalate)

type Cucumbers = A.UArray Int Bool

data Grids = Grids { 
  rs :: Int,
  cs :: Int,
  right :: Cucumbers,
  down :: Cucumbers,
  indices :: A.UArray Int Int
} deriving Eq

instance Show Grids where
  show (Grids rs cs right down indices) = intercalate "\n" $ do
    r <- [0..(rs - 1)]
    let row = [(r * cs).. ((r * cs) + cs - 1)]
    pure $ map (\i -> if down A.! i then 'v' 
              else if right A.! i then '>'
                   else '.') row

parseLine :: GenParser Char st [Char]
parseLine = many1 (oneOf ".>v") <* endOfLine

parseLines :: GenParser Char st Grids
parseLines = do
  first <- parseLine
  let cols = length first
  rest <- (many parseLine)
  let allLines = concat (first : rest)
  let rows = length rest + 1
  let isRight = (map (== '>') allLines)
  let rightGrid = A.listArray (0, rows * cols - 1) $ isRight
  let downGrid = A.listArray (0, rows * cols - 1) $ map (== 'v') allLines
  let is = A.listArray (0, rows * cols - 1) $ [0..(rows * cols - 1)]
  pure (Grids { rs = rows, cs = cols, right = rightGrid, down = downGrid, indices = is })

input :: IO (Either ParseError Grids)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day25.input"
  return $ parse parseLines "day25.input" (T.unpack fileContent)

type Rows = Int
type Cols = Int
type Row = Int
type Col = Int
type Index = Int
type Size = Int

nextRow :: Size -> Cols -> Index -> Index
nextRow size cols i = (i + cols) `mod` size

prevRow :: Size -> Cols -> Index -> Index
prevRow size cols i = (i - cols + size) `mod` size

nextCol :: Cols -> Index -> Index
nextCol cols i = ((i + 1) `mod` cols) + (i `div` cols) * cols

prevCol :: Cols -> Index -> Index
prevCol cols i = ((i - 1 + cols) `mod` cols) + (i `div` cols) * cols

nextGrids :: Grids -> Grids
nextGrids (Grids rows cols rights downs is) = 
  let size = rows * cols
      newRights = A.amap (\i -> let next = nextCol cols i
                                    prevC = prevCol cols i
                                in  (rights A.! prevC && not (rights A.! i) && not (downs A.! i))
                                    || (rights A.! i 
                                        && ((rights A.! next) ||
                                            (downs A.! next)))) is
      newDowns = A.amap (\i -> let next = nextRow size cols i
                                   prevR = prevRow size cols i
                               in (downs A.! prevR && not (downs A.! i) 
                                   && not (newRights A.! i)) ||
                                 (downs A.! i 
                                  && ((downs A.! next) ||
                                      (newRights A.! next)))) is
  in Grids rows cols newRights newDowns is
                                     
data Result = Result { gs :: Grids, turns :: Int } deriving Show

iterateGrids :: Grids -> Result
iterateGrids grs = iterateGrids' grs 1
  where iterateGrids' curr i =
          let next = (nextGrids curr) 
          in if curr == next
             then (Result next i)
             else iterateGrids' next (i + 1)


part1 :: IO (Either ParseError Result)
part1 = (fmap iterateGrids) <$> input
