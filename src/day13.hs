module Day13 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Dot = Dot Int Int deriving (Ord, Eq, Show)

data Direction = UpDir | LeftDir deriving Show

data Fold = Fold Direction Int deriving Show

data Input = Input [Dot] [Fold] deriving Show

parseDot :: GenParser Char st Dot
parseDot = do
  x <- positiveInt
  _ <- char ','
  y <- positiveInt
  return (Dot x y)

parseDots :: GenParser Char st [Dot]
parseDots = many1 (parseDot <* endOfLine)

parseFold :: GenParser Char st Fold
parseFold = string "fold along " >> 
  ((string "x=" *> ((Fold LeftDir) <$> positiveInt)) 
   <|> (string "y=" *> ((Fold UpDir) <$> positiveInt)))
  
parseFolds :: GenParser Char st [Fold]
parseFolds = many1 (parseFold <* endOfLine)

parseInput :: GenParser Char st Input
parseInput = do
  dots <- parseDots
  _ <- endOfLine
  folds <- parseFolds
  return (Input dots folds)

input :: IO (Either ParseError Input)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day13.input"
  return $ parse parseInput "day13.input" (T.unpack fileContent)

type Paper = S.Set Dot

foldPaper :: Fold -> Paper -> Paper
foldPaper (Fold UpDir r) = foldl' (\st (Dot x y) -> let newY = if y <= r then y else 2 * r - y
                                    in S.insert (Dot x newY) st) S.empty 
foldPaper (Fold LeftDir c) = foldl' (\st (Dot x y) -> let newX = if x <= c then x else 2 * c - x
                                    in S.insert (Dot newX y) st) S.empty

visibleAfterFirstFold :: Input -> Maybe Int
visibleAfterFirstFold (Input _ []) = Nothing
visibleAfterFirstFold (Input ds (f : _)) = Just $ S.size (foldPaper f (S.fromList ds))

part1 :: IO (Either ParseError (Maybe Int))
part1 = (fmap visibleAfterFirstFold) <$> input

completeFold :: Input -> Paper
completeFold (Input dots folds) = foldl' (flip foldPaper) (S.fromList dots) folds

data Coord = Coord Int Int

data Bounds = Bounds Int Int Int Int

findBounds :: Paper -> Bounds
findBounds paper = let mxX = foldl' (\acc (Dot x _) -> if x > acc then x else acc) 0 paper
                       mnX = foldl' (\acc (Dot x _) -> if x < acc then x else acc) mxX paper
                       mxY = foldl' (\acc (Dot _ y) -> if y > acc then y else acc) 0 paper
                       mnY = foldl' (\acc (Dot _ y) -> if y < acc then y else acc) mxY paper
                   in Bounds mnX mnY mxX mxY

displayLine :: Paper -> Bounds -> Int -> String
displayLine paper (Bounds mnX _ mxX _) y = 
  map (\x -> if S.member (Dot x y) paper then '#' else '.') [mnX..mxX]

displayCode :: Paper -> Bounds -> [String]
displayCode paper bs@(Bounds _ mnY _ mxY) = map (displayLine paper bs) [mnY..mxY]
  

determineCode :: Input -> [String]
determineCode inp = let p = completeFold inp
                        bounds = findBounds p
                    in displayCode p bounds

printCode :: [String] -> IO ()
printCode [] = return ()
printCode (l : rest) = putStrLn l >> printCode rest

part2 :: IO ()
part2 = do
  inp <- input
  case inp of
    Right inpt -> printCode $ determineCode inpt
    Left err -> putStrLn $ show err
