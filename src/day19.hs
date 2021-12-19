module Day19 where

import Debug.Trace
import Data.Either
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

int :: GenParser Char st Int
int = (char '-' *> ((*(-1)) <$> positiveInt)) <|> positiveInt

data Beacon = Beacon { 
  beaconX :: Int,
  beaconY :: Int,
  beaconZ :: Int
} deriving (Eq, Ord, Show)

data Scanner = Scanner Int [Beacon] deriving Show

beacon :: GenParser Char st Beacon
beacon = do
  x <- int
  _ <- char ','
  y <- int
  _ <- char ','
  z <- int
  return (Beacon x y z)

scanner :: GenParser Char st Scanner
scanner = do
  _ <- string "--- scanner "
  sid <- positiveInt
  _ <- (string " ---" >> endOfLine)
  bs <- many1 (beacon <* endOfLine)
  return (Scanner sid bs)

parseScanners :: GenParser Char st [Scanner]
parseScanners = sepBy scanner endOfLine

input :: IO (Either ParseError [Scanner])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day19.input"
  return $ parse parseScanners "day19.input" (T.unpack fileContent)

data MatchFailed = NoDifferenceChosen | DontMatch deriving Show

-- matchDifferences :: [Int] -> [Int] -> Int -> Maybe Int -> Either MatchFailed [Int]
-- matchDifferences _ _ 0 (Just d) = Right [d]
-- matchDifferences _ _ 0 Nothing = Left NoDifferenceChosen
-- matchDifferences _ [] _ _ = Left DontMatch
-- matchDifferences [] _ _ _ = Left DontMatch
-- matchDifferences is@(i : iRest) js@(j : jRest) rm Nothing = 
--   trace ("NEXT I " ++ show i ++ " NEXT J " ++ show j) $
--   let firstDiff = matchDifferences iRest jRest (rm - 1) (Just (i - j))
--       skipI = matchDifferences iRest js rm Nothing
--       skipJ = matchDifferences is jRest rm Nothing
--       allResults = [firstDiff, skipI, skipJ]
--   in case rights allResults of
--     [] -> Left (head (lefts allResults))
--     rs -> Right (concat rs)
-- matchDifferences is@(i : iRest) js@(j : jRest) rm df@(Just d) = 
--   trace ("CHOSEN D " ++ show d ++ " NEXT I " ++ show i ++ " NEXT J " ++ show j) $
--   if i - j == d 
--   then matchDifferences iRest jRest (rm - 1) df 
--   else let skipI = matchDifferences iRest js rm df
--            skipJ = matchDifferences is jRest rm df
--        in case (skipI, skipJ) of
--             (e1@(Left _), Left _) -> e1
--             (Left _, ds@(Right _)) -> ds
--             (ds@(Right _), Left _) -> ds
--             (Right ds1, Right ds2) -> Right (ds1 ++ ds2)

haveOverlappingX :: Scanner -> Scanner -> Either MatchFailed [Int]
haveOverlappingX (Scanner _ bs1) (Scanner _ bs2) = 
  let xs1 = L.sort $ map beaconX bs1
      xs2 = L.sort $ map beaconX bs2
  in matchDifferences xs1 xs2 12 Nothing

checkOverlapping :: [Scanner] -> Either MatchFailed [Int]
checkOverlapping [] = Left DontMatch
checkOverlapping [_] = Left DontMatch
checkOverlapping (i : j : _) = haveOverlappingX i j

part1  = (fmap checkOverlapping) <$> input
