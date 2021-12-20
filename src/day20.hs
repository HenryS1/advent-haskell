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

type Pixels = A.Array Int Int

type Indices = A.Array Int Int

data Image = Image Int Int Pixels Indices 

instance Show Image where
  show i = "\n" ++ imageToString i

data Input = Input Algorithm Image deriving Show

parseAlgorithm :: GenParser Char st Algorithm
parseAlgorithm = do 
  alg <- many1 (oneOf ['#', '.'])
  return (A.listArray (0, length alg - 1) alg)

parseRow :: GenParser Char st String
parseRow = (\s -> "....." ++ s ++ ".....") <$> many1 (oneOf ['#', '.'])

parseImage :: GenParser Char st Image
parseImage = do
  first <- (parseRow <* endOfLine)
  rest <- many1 (parseRow <* endOfLine)
  let cs = length first
  let emptyRow = take cs $ repeat '.'
  let rs = length rest + 11
  let allRows = concat ((emptyRow : emptyRow : emptyRow : emptyRow : emptyRow : first : rest) ++ [emptyRow, emptyRow, emptyRow, emptyRow, emptyRow])
  let indices = A.listArray (0, rs * cs - 1) $ take (rs * cs) [0..]
  return $ Image rs cs (A.listArray (0, (rs * cs) - 1) $ map (\c -> if c == '.' then 0 else 1) allRows) indices

toString' :: Int -> String -> String
toString' _ "" = ""
toString' cs s = take cs s ++ "\n" ++ toString' cs (drop cs s)    

imageToString :: Image -> String
imageToString (Image _ cs ps _) = 
  toString' cs (map (\i -> if i == 1 then '#' else '.') $ A.elems ps)


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

bitsToInt :: [Int] -> Int
bitsToInt bs = bitsToInt' 0 bs
  where bitsToInt' acc [] = acc
        bitsToInt' acc (b : rest) = bitsToInt' ((acc * 2) + b) rest

closeBy :: Image -> Int -> Int -> Bool
closeBy (Image _ cs _ _) i j = let (r1, c1) = i `divMod` cs
                                   (r2, c2) = j `divMod` cs
                               in abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

neighbours :: Image -> Int -> [Int]
neighbours im@(Image rs cs _ _) i = filter (\j -> closeBy im i j) $
  [i - cs - 1, i - cs, i - cs + 1, i - 1, i, i + 1, i + cs - 1, i + cs, i + cs + 1]

pixelCoord :: Image -> Int -> Int
pixelCoord img@(Image rs cs ps _) i = 
  let bits = map (\n -> if n >= 0 && n < (rs * cs - 1) then ps A.! n else 0) $ neighbours img i
  in --trace ("BITS " ++ show bits) $
    bitsToInt bits

nextImage :: Algorithm -> Image -> Image
nextImage alg img@(Image rs cs _ is) = 
  let newPs = fmap (\i -> let pc = pixelCoord img i
                          in --trace ("I " ++ show i ++ " PC " ++ show pc ++ " ALG PC " ++ show (alg A.! pc)) $
                             if alg A.! pc == '.' then 0 else 1) is
  in Image rs cs newPs is

iterImage :: Algorithm -> Image -> Int -> Image
iterImage _ img 0 = img
iterImage alg img i = iterImage alg (nextImage alg img) (i - 1)

--part1 :: IO (Either ParseError Image)
part1 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let (Image rs cs ps is) = iterImage alg img 2
                             in Right (foldl' (\total i -> let (r, c) = i `divMod` cs
                                                           in if c == 0 || c == cs - 1 || r == 0 || r == rs - 1 
                                                              then total else 
                                                  ps A.! i + total) 0 is)

part2 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let im@(Image rs cs ps is) = iterImage alg img 6
                             in Right (foldl' (+) 0 ps)
