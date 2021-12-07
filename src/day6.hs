module Day6 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

type LanternFish = M.Map Int Int

accumulateFish :: [Int] -> M.Map Int Int
accumulateFish = foldl' (\mp i -> M.insertWith (+) i 1 mp) M.empty

parseLanternFish :: GenParser Char st LanternFish
parseLanternFish = do
  is <- sepBy1 positiveInt (char ',')
  return $ accumulateFish is

input :: IO (Either ParseError LanternFish)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day6.input"
  return $ parse parseLanternFish "day6.input" (T.unpack fileContent)

tick :: LanternFish -> LanternFish
tick lf = foldl' (\newLf i -> case M.lookup i lf of
                     Nothing -> newLf
                     Just n -> 
                       let shifted = M.insert ((i - 1 + 7) `mod` 7) n newLf
                       in if i == 0 
                          then M.insertWith (+) 8 n shifted
                          else if i == 7 || i == 8
                               then M.insertWith (+) (i - 1) n newLf
                               else shifted) M.empty [0..8]

totalFish :: LanternFish -> Int
totalFish = foldl' (+) 0

fishAfterNTurns :: Int -> LanternFish -> LanternFish
fishAfterNTurns turns curr = if turns == 0 then curr 
  else fishAfterNTurns (turns - 1) (tick curr)

part1 :: IO (Either ParseError Int)
part1 = (fmap (totalFish . (fishAfterNTurns 80))) <$> input
  
part2 :: IO (Either ParseError Int)
part2 = (fmap (totalFish . (fishAfterNTurns 256))) <$> input
