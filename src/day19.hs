module Day19 where

import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

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

data CoordType = X | Y | Z deriving (Eq, Show)

data Difference = Difference { 
  differenceD :: Int,
  differenceFlip :: Int,
  differenceCoordOne :: CoordType,
  differenceCoordOther :: CoordType
}

instance Show Difference where
  show (Difference d f c1 c2) = "(Difference " ++ show d ++ " " ++ show f ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"

type Differences = [Difference]

findMatches :: Differences -> [Int] -> [Int] -> Differences
findMatches ds is js = filter (hasMatch is js) ds
 
hasMatch :: [Int] -> [Int] -> Difference -> Bool
hasMatch is js (Difference d _ _ _) = let diffed = map (\i -> i - d) is
                                      in if length (diffed `L.intersect` js) >= 12
                                         then --trace ("IS " ++ show is ++ " INTERSECTION " ++ show (diffed `L.intersect` js) ++ " D " ++ show d) $ True
                                           True
                                         else False

differencesBetween :: [Int] -> [Int] -> [Int]
differencesBetween is js = [i - j | i <- is, j <- js]

allDifferences :: [Int] -> [Int] -> [Int]
allDifferences is js = S.toList $ S.fromList $ differencesBetween is js

flipOrientation :: [Int] -> [Int]
flipOrientation is = map (*(-1)) is

overlapDiffs :: CoordType -> CoordType -> [Int] -> [Int] -> [Difference]
overlapDiffs fstC sndC is js = 
  let diffs = allDifferences is js
      flipped = flipOrientation js
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
  (_, _, d1, d2, d3) <- matchesForPair s1 s2 xDs
  return (scannerId s2, scannerId s1, d1, d2, d3)

findPathToZero :: Int -> Transforms -> Maybe [Transform]
findPathToZero i offsets = findTransforms' S.empty [] i
  where findTransforms' :: S.Set Int -> [Transform] -> Int -> Maybe [Transform]
        findTransforms' seen acc curr = 
          if curr == 0 then Just acc
          else case M.lookup curr offsets of
            Nothing -> Nothing
            Just ts -> let unseen = filter (\tr -> not $ S.member (transformEnd tr) seen) ts
                           newSeen = foldl' (\sn o -> S.insert (transformEnd o) sn) seen unseen
                       in case find isJust $ map (\o -> findTransforms' newSeen (o : acc) (transformEnd o)) unseen of
                            Nothing -> Nothing
                            Just path -> path

findTransformPaths :: [(Int, Int, Difference, Difference, Difference)] -> [(Int, Maybe [Transform])]
findTransformPaths pairOffsets = 
  let offsets = mapTransforms pairOffsets
      coords = S.toList $ foldl' (\st (i, j, _, _, _) -> S.insert i $ S.insert j st) S.empty pairOffsets
  in map (\i -> (i, findPathToZero i offsets)) coords

dotVecs :: [Int] -> [Int] -> Int
dotVecs xs ys = sum $ zipWith (*) xs ys

matTimesVec :: [[Int]] -> [Int] -> [Int]
matTimesVec m v = map (\r -> dotVecs r v) m

matTimesMat :: [[Int]] -> [[Int]] -> [[Int]]
matTimesMat m1 m2 = L.transpose $ map (matTimesVec m1) (L.transpose m2)

addVectors :: [Int] -> [Int] -> [Int]
addVectors = zipWith (+)

inv :: [[Int]] -> [[Int]]
inv = L.transpose

data Transform = Transform Int Int [[Int]] [Int] deriving Show

transformEnd :: Transform -> Int
transformEnd (Transform _ e _ _) = e

composeTransforms :: Transform -> Transform -> Transform
composeTransforms (Transform _ end m2 v2) (Transform start _ m1 v1) = 
  Transform start end (matTimesMat m2 m1) (addVectors (matTimesVec m2 v1) v2)

scalarTimesVec :: Int -> [Int] -> [Int]
scalarTimesVec i = map (*i)

matFromDiffs :: Difference -> Difference -> Difference -> [[Int]]
matFromDiffs d1 d2 d3 = 
  let r1 = case (differenceCoordOne d1, differenceCoordOther d1) of
        (X, X) -> [1, 0, 0]
        (X, Y) -> [0, 1, 0]
        (X, Z) -> [0, 0, 1]
      r2 = case (differenceCoordOne d2, differenceCoordOther d2) of
        (Y, X) -> [1, 0, 0]
        (Y, Y) -> [0, 1, 0]
        (Y, Z) -> [0, 0, 1]
      r3 = case (differenceCoordOne d3, differenceCoordOther d3) of
        (Z, X) -> [1, 0, 0]
        (Z, Y) -> [0, 1, 0]
        (Z, Z) -> [0, 0, 1]
  in [scalarTimesVec (differenceFlip d1) r1,
      scalarTimesVec (differenceFlip d2) r2,
      scalarTimesVec (differenceFlip d3) r3]

makeTransform :: (Int, Int, Difference, Difference, Difference) -> Transform
makeTransform (s, e, d1, d2, d3) = 
  let vec = [differenceD d1, differenceD d2, differenceD d3]
      mat = matFromDiffs d1 d2 d3
  in Transform s e mat vec

inverse :: Transform -> Transform
inverse (Transform s e m v) = 
  let matInv = inv m
  in (Transform e s matInv (scalarTimesVec (-1) (matTimesVec matInv v)))

type Transforms = M.Map Int [Transform]

mapTransforms :: [(Int, Int, Difference, Difference, Difference)] -> Transforms
mapTransforms = foldl' (\mp o@(one, other, _, _, _) ->
                          let tr = makeTransform o
                              invTr = inverse tr
                          in M.insertWith (++) one [tr]
                             $ M.insertWith (++) other [invTr] mp) M.empty

composeAllTransforms :: [Transform] -> Maybe Transform
composeAllTransforms [] = Nothing
composeAllTransforms ts@(_ : _) = Just $ foldr1 composeTransforms ts

type TransformsToZero = M.Map Int (Maybe Transform)

transformsToZero :: [(Int, Int, Difference, Difference, Difference)] -> TransformsToZero
transformsToZero pairOffsets = 
  let paths = findTransformPaths pairOffsets
  in M.fromList $ map (\(i, trs) -> (i, trs >>= composeAllTransforms)) paths

applyTransform :: Transform -> [Int] -> [Int]
applyTransform (Transform _ _ m v) b = addVectors (matTimesVec m b) v

beaconCoords :: Beacon -> [Int]
beaconCoords (Beacon x y z) = [x, y, z]

takeCoordinatesToZero :: Scanner -> M.Map Int (Maybe Transform) -> Maybe [[Int]]
takeCoordinatesToZero s ts = case M.lookup (scannerId s) ts of
  Nothing -> Nothing
  Just Nothing -> Just (map beaconCoords (scannerBeacons s))
  Just (Just tr) -> Just $ map (applyTransform tr) $ map beaconCoords (scannerBeacons s)

coordsAtZero :: [Scanner] -> M.Map Int (Maybe Transform) -> S.Set [Int]
coordsAtZero scns ts = foldl' (\st sc -> case takeCoordinatesToZero sc ts of
                                  Nothing -> st
                                  Just crds -> S.union st (S.fromList crds)) S.empty scns

answer :: [Scanner] -> S.Set [Int]
answer scns = 
  let ts = transformsToZero $ allMatches scns
  in coordsAtZero scns ts

part1 :: IO (Either ParseError Int)
part1 = (fmap (S.size . answer)) <$> input

data ScannerCoord = ScannerCoord Int [Int] deriving Show

scannerCoords :: TransformsToZero -> [[Int]]
scannerCoords ts = map (\(_, tr) -> case tr of 
                           Nothing -> [0, 0, 0]
                           Just (Transform _ _ _ offset) -> offset) $ M.toList ts

manhattanDistance :: [Int] -> [Int] -> Int
manhattanDistance one other = sum $ zipWith (\a b -> abs (a - b)) one other

maximumManhattanDistance :: [[Int]] -> Int
maximumManhattanDistance = maximum . map (uncurry manhattanDistance) . chooseTwo

part2 :: IO (Either ParseError Int)
part2 = (fmap (maximumManhattanDistance . scannerCoords . transformsToZero . allMatches)) <$> input
