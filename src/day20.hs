module Day20 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Array.Unboxed as A

positiveInt :: GenParser Char st Word8
positiveInt = read <$> many1 digit

type Algorithm = A.UArray Int Char

type Pixels = A.UArray Word16 Word8

type Indices = A.UArray Word16 Word16

data Image = Image Word16 Word16 Pixels Indices 

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
parseRow = (\s -> repeatA 51 '.' ++ s ++ repeatA 51 '.') <$> many1 (oneOf ['#', '.'])

parseImage :: GenParser Char st Image
parseImage = do
  first <- (parseRow <* endOfLine)
  rest <- many1 (parseRow <* endOfLine)
  let cs = length first
  let emptyRow = take cs $ repeat '.'
  let rs = length rest + 103
  let allRows = concat ((repeatA 51 emptyRow ++ (first : rest)) ++ (repeatA 51 emptyRow))
  let indices = A.listArray (0, fromIntegral (rs * cs - 1)) $ take (rs * cs) [0..]
  return $ Image (fromIntegral rs) (fromIntegral cs) (A.listArray (0, fromIntegral (rs * cs) - 1) $ map (\c -> if c == '.' then 0 else 1) allRows) indices

toString' :: Word16 -> String -> String
toString' _ "" = ""
toString' cs s = take (fromIntegral cs) s ++ "\n" ++ toString' cs (drop (fromIntegral cs) s)    

imageToString :: Image -> String
imageToString (Image _ cs ps _) = 
  toString' (fromIntegral cs) (map (\i -> if i == 1 then '#' else '.') $ A.elems ps)


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

bitsToInt :: [Word8] -> Word16
bitsToInt bs = bitsToInt' 0 bs
  where bitsToInt' :: Word16 -> [Word8] -> Word16
        bitsToInt' acc [] = acc
        bitsToInt' acc (b : rest) = bitsToInt' ((acc * 2) + (fromIntegral b)) rest

closeBy :: Image -> Int -> Int -> Bool
closeBy (Image _ cs _ _) i j = let (r1, c1) = i `divMod` (fromIntegral cs)
                                   (r2, c2) = j `divMod` (fromIntegral cs)
                               in abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

neighbours :: Image -> Word16 -> [Word16]
neighbours (Image _ cs _ _) i = 
  [i - cs - 1, i - cs, i - cs + 1, i - 1, i, i + 1, i + cs - 1, i + cs, i + cs + 1]

pixelCoord :: Word8 -> Image -> Word16 -> Word16
pixelCoord turn img@(Image rs cs ps _) i = 
  let bits = map (\n -> let (r, c) = n `divMod` cs        
                        in if r >= 0 && r < rs && c >= 0 && c < cs
                           then ps A.! n else 
                             if turn `mod` 2 == 0 then 0 else 1) $ neighbours img i
  in bitsToInt bits

nextImage :: Word8 -> Algorithm -> Image -> Image
nextImage turn alg img@(Image rs cs _ is) = 
  let newPs = A.amap (\i -> let pc = pixelCoord turn img i
                          in if alg A.! (fromIntegral pc) == '#'
                             then 1 else 0) is
  in Image rs cs newPs is

iterImage :: Algorithm -> Image -> Word8 -> Word8 -> Image
iterImage _ img 0 _ = img
iterImage alg img i turn = iterImage alg (nextImage turn alg img) (i - 1) (turn + 1)

part1 :: IO (Either ParseError Word16)
part1 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let (Image rs cs ps _) = iterImage alg img 2 0
                             in Right (foldl' (\total i -> (fromIntegral $ ps A.! i) + total) 0 [0..(rs * cs) - 1])

part2 :: IO (Either ParseError Word16)
part2 = do
  parsedInput <- input
  return $ case parsedInput of
    Left err -> Left err
    Right (Input alg img) -> let (Image rs cs ps _) = iterImage alg img 50 0
                             in Right (foldl' (\total i -> (fromIntegral $ ps A.! i) + total) 0 [0..(rs * cs) - 1])

