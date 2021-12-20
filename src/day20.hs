module Day20 where

import Debug.Trace
import Data.Maybe
import Data.Foldable
import Data.Either
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

type Algorithm = A.Array Int Char

data Coord = Coord Int Int deriving (Ord, Eq, Show)

data Image = Image Int Int Int Int (S.Set Coord) deriving Show

data Input = Input Algorithm Image deriving Show

parseAlgorithm :: GenParser Char st Algorithm
parseAlgorithm = do 
  alg <- many1 (oneOf ['#', '.'])
  return (A.listArray (0, length alg - 1) alg)

parseRow :: GenParser Char st String
parseRow = many1 (oneOf ['#', '.'])

parseImage :: GenParser Char st Image
parseImage = do
  first <- (parseRow <* endOfLine)
  rest <- many1 (parseRow <* endOfLine)
  let cs = length first
  let rs = length rest + 1
  let allRows = first : rest
  let withRow = zip [0..rs - 1] allRows
  let withCol = map (\(i, r) -> (i, zip [0..] r)) withRow
  let withCoord = map (\(i, r) -> zip (repeat i) r) withCol
  let justCoords = map (\(r, (c, _)) -> Coord r c) $ filter (\(_, (_, ch)) -> ch == '#') $ concat withCoord
  return (Image 0 (rs - 1) 0 (cs - 1) (S.fromList justCoords))

parseInput :: GenParser Char st Input
parseInput = do
  alg <- parseAlgorithm
  _ <- (endOfLine >> endOfLine)
  img <- parseImage
  return (Input alg img)

input :: IO (Either ParseError Input)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day20.input"
  return $ parse parseInput "day20.input" (T.unpack fileContent)

neighbours :: Coord -> [Coord]
neighbours (Coord r c) = [Coord (r - 1) (c - 1), Coord (r - 1) c, Coord (r - 1) (c + 1), 
                          Coord r (c - 1), Coord r c, Coord r (c + 1),
                          Coord (r + 1) (c - 1), Coord (r + 1) c, Coord (r + 1) (c + 1)]

bitsToInt :: [Int] -> Int
bitsToInt bs = bitsToInt' 0 bs
  where bitsToInt' acc [] = acc
        bitsToInt' acc (b : rest) = bitsToInt' ((acc * 2) + b) rest

findPixelCoordinate :: Image -> Coord -> Int
findPixelCoordinate (Image _ _ _ _ ps) crd = 
  let ns = neighbours crd
  in bitsToInt $ map (\c -> if S.member c ps then 1 else 0) ns

nextImage :: Algorithm -> Image -> Image
nextImage alg im@(Image mnr mxr mnc mxc _) = 
  let rs = mxr - mnr + 1
      cs = mxc - mnc + 1
      (newPs, nMnr, nMnc, nMxr, nMxc) = 
        foldl' (\acc@(st, newMnr, newMnc, newMxr, newMxc) i -> 
               let (rInit, cInit) = i `divMod` (cs + 4)
                   r = rInit + mnr - 2
                   c = cInit + mnc - 2
                   nextCrd = (Coord r c)
                   pixelCoord = findPixelCoordinate im nextCrd
                   isMember = alg A.! pixelCoord == '#'
               in if isMember 
               then-- trace ("NEXT CRD " ++ show nextCrd) $
                    let nextMnr = newMnr `min` r
                        nextMnc = newMnc `min` c
                        nextMxr = newMxr `max` r
                        nextMxc = newMxc `max` c
                        nextSt = S.insert nextCrd st
                    in (nextSt, nextMnr, nextMnc, nextMxr, nextMxc)
               else acc) (S.empty, mnr, mxr, mnc, mxc) [0..((rs + 4) * (cs + 4) - 1)]
  in Image (mnr - 2) (mxr + 2) (mnc - 2) (mxc + 2) newPs

iterImage :: Algorithm -> Image -> Int -> Image
iterImage _ img 0 = img
iterImage alg img i = iterImage alg (nextImage alg img) (i - 1)

--part1 :: IO (Either ParseError Int)
part1 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> 
      let (Image _ _ _ _ ps) = (iterImage alg img 1)
      in Right ps
