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

matchWithDifference :: S.Set Int -> [Int] -> Int -> Either MatchFailed Int
matchWithDifference is js d = 
  let other = S.fromList $ map (\a -> a - d) js 
  in if S.size (S.intersection is other) >= 12 then Right d else Left DontMatch

data CoordType = X | Y | Z deriving Show

data Difference = Difference { 
  differenceD :: Int,
  differenceFlip :: Int,
  differenceCoordOne :: CoordType,
  differenceCoordOther :: CoordType
}

instance Show Difference where
  show (Difference d f c1 c2) = "(" ++ show d ++ "," ++ show f ++ "," ++ show c1 ++ "," ++ show c2 ++ ")"

type Differences = [Difference]

findMatches :: Differences -> [Int] -> [Int] -> Differences
findMatches ds is js = filter (hasMatch is js) ds
 
hasMatch :: [Int] -> [Int] -> Difference -> Bool
hasMatch is js (Difference d _ _ _) = let diffed = map (\j -> j - d) js
                                      in length (diffed `L.intersect` is) >= 12

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

overlapDiffs :: CoordType -> CoordType -> [Int] -> [Int] -> [Difference]
overlapDiffs fstC sndC is js = 
  let diffs = allDifferences is js
      flipped = flipOrientation is
      diffsFlipped = allDifferences is flipped
  in findMatches (map (\i -> (Difference i 1 fstC sndC)) diffs) is js 
     ++ findMatches (map (\i -> (Difference i (-1) fstC sndC)) diffsFlipped) is flipped

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

coordByType :: CoordType -> Scanner -> [Int]
coordByType X = map beaconX . scannerBeacons
coordByType Y = map beaconY . scannerBeacons
coordByType Z = map beaconZ . scannerBeacons

xMatches :: (Scanner, Scanner) -> (Scanner, Scanner, [Difference])
xMatches (one, other) = 
  let xs1 = (map beaconX (scannerBeacons one))
      allowedDiffs = overlapDiffs X X xs1 (map beaconX (scannerBeacons other))
        ++ overlapDiffs X Y xs1 (map beaconY (scannerBeacons other))
        ++ overlapDiffs X Z xs1 (map beaconZ (scannerBeacons other))
  in (one, other, allowedDiffs)
  
findXMatches :: [Scanner] -> [(Scanner, Scanner, [Difference])]
findXMatches = filter (\(_, _, ds) -> not $ null ds) . map xMatches . chooseTwo

findYMatches :: Difference -> Scanner -> Scanner -> [Difference]
findYMatches d s1 s2 = 
  let ys1 = map beaconY (scannerBeacons s1)
      rm = case differenceCoordOther d of
        X -> [Y, Z]
        Y -> [X, Z]
        Z -> [X, Y]
  in rm >>= (\tp -> overlapDiffs Y tp ys1 (coordByType tp s2))

findZMatches :: Difference -> Difference -> Scanner -> Scanner -> [Difference]
findZMatches xD yD s1 s2 =
  let zs1 = coordByType Z s1
      tp = case (differenceCoordOther xD, differenceCoordOther yD) of
        (X, Y) -> Z
        (Y, X) -> Z
        (X, Z) -> Y
        (Z, X) -> Y
        (Y, Z) -> X
        (Z, Y) -> X
  in overlapDiffs Z tp zs1 (coordByType tp s2)

matchesForPair :: Scanner -> Scanner -> [Difference] -> [(Scanner, Scanner, Difference, Difference, Difference)]
matchesForPair s1 s2 ds = do
  xD <- ds
  yD <- findYMatches xD s1 s2
  zD <- findZMatches xD yD s1 s2
  return (s1, s2, xD, yD, zD)

allMatches :: [Scanner] -> [(Int, Int, Difference, Difference, Difference)]
allMatches scns = do
  (s1, s2, xDs) <- findXMatches scns
  (s1, s2, d1, d2, d3) <- matchesForPair s1 s2 xDs
  return (scannerId s1, scannerId s2, d1, d2, d3)

part1  = (fmap (map (\(s1, s2, ds) -> (scannerId s1, scannerId s2, ds)) . findXMatches)) <$> input
