module Day19 where

import Debug.Trace
import Data.Either
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Set as S

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

int :: GenParser Char st Int
int = (char '-' *> ((*(-1)) <$> positiveInt)) <|> positiveInt

data Beacon = Beacon { 
  beaconX :: Int,
  beaconY :: Int,
  beaconZ :: Int
} deriving (Eq, Ord, Show)

data Scanner = Scanner { scannerId :: Int, scannerBeacons :: [Beacon] } deriving Show

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

matchWithDifference :: S.Set Int -> [Int] -> Int -> Either MatchFailed Int
matchWithDifference is js d = 
  let other = S.fromList $ map (\a -> a - d) js 
  in if S.size (S.intersection is other) >= 12 then Right d else Left DontMatch

-- matchDifferences :: [Int] -> [Int] -> ([Int], S.Set Int)
-- matchDifferences one other = match' one other S.empty
--   where oneSet = S.fromList one
--         match' [] _ used = ([], used)
--         match' _ [] used = ([], used)
--         match' is@(i : iRest) js@(j : jRest) alreadyUsed = 
--           trace ("NEXT I " ++ show i ++ " NEXT J " ++ show j) $
--           let matchFirst = if S.member (j - i) alreadyUsed 
--                 then Left DontMatch
--                 else matchWithDifference oneSet js (j - i)
--               (ds, nextUsed) = case matchFirst of
--                 Left _ -> ([], S.insert (j - i) alreadyUsed)
--                 Right v -> ([v], S.insert (j - i) alreadyUsed) 
--               (skipIDs, skipIUsed) = match' iRest js nextUsed
--               (skipJDs, skipJUsed) = match' is jRest skipIUsed
--           in (ds ++ skipIDs ++ skipJDs, skipJUsed)
              -- skipI = match' iRest js 
              -- skipJ = match' is jRest

type Differences = [Int]

findMatches :: Differences -> [Int] -> [Int] -> Differences
findMatches ds is js = filter (hasMatch is js) ds
 
hasMatch :: [Int] -> [Int] -> Int -> Bool
hasMatch is js d = let diffed = map (\j -> j - d) js
                   in length (diffed `L.intersect` is) >= 11

differencesBetween :: [Int] -> [Int] -> [Int]
differencesBetween is js = [j - i | i <- is, j <- js]

allDifferences :: [Int] -> [Int] -> [Int]
allDifferences is js = S.toList $ S.fromList $ differencesBetween is js
-- allDifferences [] _ = S.empty
-- allDifferences _ [] = S.empty
-- allDifferences is@(i : iRest) js@(j : jRest) = S.insert (i - j) $ allDifferences iRest jRest `S.union` allDifferences is

flipOrientation :: [Int] -> [Int]
flipOrientation is = map (*(-1)) is

-- haveOverlappingX :: Scanner -> Scanner -> [Int]
-- haveOverlappingX (Scanner _ bs1) (Scanner _ bs2) = 
--   let xs1 = L.sort $ map beaconX bs1
--       xs2 = L.sort $ map beaconX bs2
--       diffs = S.toList $ allDifferences xs1 xs2
--   in findMatches diffs xs1 xs2 ++ findMatches diffs xs1 (flipOrientation xs2)

overlapDiffs :: [Int] -> [Int] -> [Int]
overlapDiffs is js = 
  let diffs = allDifferences is js
      flipped = flipOrientation is
      diffsFlipped = allDifferences is flipped
  in findMatches diffs is js ++ findMatches diffsFlipped is flipped

chooseTwo :: [a] -> [(a, a)]
chooseTwo as = chooseTwo' Nothing as
  where chooseTwo' _ [] = []
        chooseTwo' Nothing [_] = []
        chooseTwo' Nothing (a : rest) = chooseTwo' (Just a) rest ++ chooseTwo' Nothing rest
        chooseTwo' j@(Just one) (other : rest) =
          (one, other) : (chooseTwo' j rest)

-- checkOverlapping :: [Scanner] -> Either MatchFailed [Int]
-- checkOverlapping [] = Left DontMatch
-- checkOverlapping [_] = Left DontMatch
-- checkOverlapping (i : j : _) = Right (haveOverlappingX i j)

xMatches :: (Scanner, Scanner) -> (Scanner, Scanner, [Int])
xMatches (one, other) = 
  let xs1 = (map beaconX (scannerBeacons one))
      allowedDiffs = overlapDiffs xs1 (map beaconX (scannerBeacons other))
        ++ overlapDiffs xs1 (map beaconY (scannerBeacons other))
        ++ overlapDiffs xs1 (map beaconZ (scannerBeacons other))
  in (one, other, allowedDiffs)
  
findXMatches :: [Scanner] -> [(Int, Int, [Int])]
findXMatches = (map (\(s1, s2, ds) -> (scannerId s1, scannerId s2, ds))) . filter (\(_, _, ds) -> not $ null ds) . map xMatches . chooseTwo

part1  = (fmap findXMatches) <$> input
