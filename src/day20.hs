module Day20 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

repeatA :: Int -> a -> [a]
repeatA i ch = take i $ repeat ch

parseRow :: GenParser Char st String
parseRow = (\s -> repeatA 52 '.' ++ s ++ repeatA 52 '.') <$> many1 (oneOf ['#', '.'])

parseImage :: GenParser Char st Image
parseImage = do
  first <- (parseRow <* endOfLine)
  rest <- many1 (parseRow <* endOfLine)
  let cs = length first
  let emptyRow = take cs $ repeat '.'
  let rs = length rest + 105
  let allRows = concat ((repeatA 52 emptyRow ++ (first : rest)) ++ (repeatA 52 emptyRow))
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
neighbours (Image _ cs _ _) i = 
  [i - cs - 1, i - cs, i - cs + 1, i - 1, i, i + 1, i + cs - 1, i + cs, i + cs + 1]

pixelCoord :: Int -> Image -> Int -> Int
pixelCoord turn img@(Image rs cs ps _) i = 
  let bits = map (\n -> let (r, c) = n `divMod` cs        
                        in if r >= 0 && r < rs && c >= 0 && c < cs
                           then ps A.! n else 
                             if turn `mod` 2 == 0 then 0 else 1) $ neighbours img i
  in bitsToInt bits

nextImage :: Int -> Algorithm -> Image -> Image
nextImage turn alg img@(Image rs cs _ is) = 
  let newPs = fmap (\i -> let pc = pixelCoord turn img i
                          in if alg A.! pc == '#'
                             then 1 else 0) is
  in Image rs cs newPs is

iterImage :: Algorithm -> Image -> Int -> Int -> Image
iterImage _ img 0 _ = img
iterImage alg img i turn = iterImage alg (nextImage turn alg img) (i - 1) (turn + 1)

part1 :: IO (Either ParseError Int)
part1 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let (Image _ _ ps _) = iterImage alg img 2 0
                             in Right (foldl' (+) 0 ps)

part2 :: IO (Either ParseError Int)
part2 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let (Image _ _ ps _) = iterImage alg img 50 0
                             in Right (foldl' (+) 0 ps)

