module Day1 where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

positiveInts :: GenParser Char st [Int]
positiveInts = many1 (positiveInt <* endOfLine)

input :: IO (Either ParseError [Int])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day1.input"
  return $ parse positiveInts "day1.input" (T.unpack fileContent)

increases :: [Int] -> Int
increases [] = 0
increases [_] = 0
increases (x : tl@(y : _)) = if x < y then 1 + increases tl else increases tl

part1 :: IO (Either ParseError Int)
part1 = (fmap increases) <$> input

slidingWindow :: [Int] -> Int
slidingWindow [] = 0
slidingWindow [_] = 0
slidingWindow [_, _] = 0
slidingWindow (x : tl@(y : z : _)) = slidingWindowIncreases (x + y + z) tl
  where slidingWindowIncreases :: Int -> [Int] -> Int
        slidingWindowIncreases _ [] = 0
        slidingWindowIncreases _ [_] = 0
        slidingWindowIncreases _ [_, _] = 0
        slidingWindowIncreases prev (a : rest@(b : c : _)) = 
          let newSum = a + b + c
          in if prev < newSum then 1 + slidingWindowIncreases newSum rest
          else slidingWindowIncreases newSum rest
  
part2 :: IO (Either ParseError Int)
part2 = (fmap slidingWindow) <$> input
