module Day1 where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

input :: IO (Either ParseError Int)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day1.input"
  return $ parse positiveInt "day1.input" (T.unpack fileContent)
