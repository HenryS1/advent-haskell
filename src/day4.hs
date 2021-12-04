module Day4 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Bits


positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Board = Board { marked :: Int, numbers :: M.Map Int Int } deriving Show

data Input = Input [Int] [Board] deriving Show

chosenNumbers :: GenParser Char st [Int]
chosenNumbers = sepBy1 positiveInt (char ',')

parseRow :: GenParser Char st [Int]
parseRow = many (char ' ') *> sepBy1 positiveInt (many1 (char ' '))

board :: GenParser Char st Board
board = do
  rows <- many1 (parseRow <* endOfLine)
  let boardNums = foldr (++) [] rows
  return (Board 0 (M.fromList (boardNums `zip` [0..24])))    

parseInput :: GenParser Char st Input
parseInput = do
  ns <- (chosenNumbers <* endOfLine)
  _ <- endOfLine
  bs <- sepBy1 board endOfLine
  return (Input ns bs)

input :: IO (Either ParseError Input)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day4.input"
  return $ parse parseInput "day4.input" (T.unpack fileContent)

markSquare :: Int -> Board -> Board
markSquare i b@(Board m lkp) = case M.lookup i lkp of
  Nothing -> b
  Just j -> Board (m .|. (shiftL 1 j)) lkp

rowIsComplete :: Board -> Int -> Bool
rowIsComplete (Board m _) i = (shiftR m (5 * i)) .&. 31 == 31

colMask :: Int
colMask = 1 .|. (shiftL 1 5) .|. (shiftL 1 10) .|. (shiftL 1 15) .|. (shiftL 1 20)

colIsComplete :: Board -> Int -> Bool
colIsComplete (Board m _) i = 
  let msk = shiftL colMask i
  in m .&. msk == msk

boardWins :: Board -> Bool
boardWins b = not $ null $ filter (\i -> rowIsComplete b i || colIsComplete b i) [0..4]

isMarked :: Int -> Board -> Bool
isMarked i (Board m _) = (m .&. (shiftL 1 i)) /= 0

unmarkedNumbers :: Board -> [Int]
unmarkedNumbers b@(Board _ lkp) = 
  M.foldrWithKey (\n i ns -> if isMarked i b then ns else n : ns) [] lkp

score :: Board -> Int -> Int
score b i = sum (unmarkedNumbers b) * i

turn :: [Board] -> [Int] -> Either ([Board], [Int]) (Maybe (Int, Board))
turn [] _ = Right Nothing
turn _ [] = Right Nothing
turn bs (i : is) = 
  let newBs = map (markSquare i) bs
  in case find boardWins newBs of
    Nothing -> (Left (newBs, is))
    Just b -> Right (Just (i, b))

game :: [Board] -> [Int] -> Maybe Int
game [] _ = Nothing
game _ [] = Nothing
game bs is = case turn bs is of
  Left (newBs, newIs) -> game newBs newIs
  Right (Just (lastNumber, b)) -> Just (score b lastNumber)
  Right Nothing -> Nothing

part1 :: IO (Either ParseError (Maybe Int))
part1 = do
  inp <- input
  case inp of 
    Left err -> return $ Left err
    Right (Input is bs) -> return $ Right (game bs is)

continueUntilFinished :: Board -> [Int] -> Maybe (Board, Int)
continueUntilFinished _ [] = Nothing
continueUntilFinished b (i : is) = 
  let newB = markSquare i b
  in if boardWins newB then Just (newB, i) else continueUntilFinished newB is

lastBoardToWin :: [Board] -> [Int] -> Maybe (Board, Int)
lastBoardToWin [] _ = Nothing
lastBoardToWin _ [] = Nothing
lastBoardToWin bs (i : is) = 
  let newBs = map (markSquare i) bs
  in case filter (not . boardWins) newBs of
    [] -> Nothing
    [b] -> continueUntilFinished b is
    remaining -> lastBoardToWin remaining is

part2 :: IO (Either ParseError (Maybe (Board, Int, Int)))
part2 = do 
  inp <- input
  case inp of
    Left err -> return $ Left err
    Right (Input is bs) -> case (lastBoardToWin bs is) of
      Nothing -> return $ Right Nothing
      Just (b, i) -> return $ Right $ Just (b, i, (score b i))
