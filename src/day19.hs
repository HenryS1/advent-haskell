module Day19 where

import Debug.Trace
import Data.Maybe
import Data.Foldable
import Data.Either
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
                                      in length (diffed `L.intersect` js) >= 12

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

data Offset = Offset Int Difference Difference Difference deriving Show

offsetOther :: Offset -> Int
offsetOther (Offset other _ _ _) = other

type Offsets = M.Map Int [Offset]

allMatches :: [Scanner] -> [(Int, Int, Difference, Difference, Difference)]
allMatches scns = do
  (s1, s2, xDs) <- findXMatches scns
  (_, _, d1, d2, d3) <- matchesForPair s1 s2 xDs
  return (scannerId s1, scannerId s2, d1, d2, d3)

flipDifference :: Difference -> Difference
flipDifference (Difference d f c1 c2) = Difference (-d) f c1 c2

findXDiff :: [Difference] -> Maybe Difference
findXDiff = L.find ((==X) . differenceCoordOther)

findYDiff :: [Difference] -> Maybe Difference
findYDiff = L.find ((==Y) . differenceCoordOther)

findZDiff :: [Difference] -> Maybe Difference
findZDiff = L.find ((==Z) . differenceCoordOther)

flipOffset :: (Int, Int, Difference, Difference, Difference) -> Maybe (Int, Int, Difference, Difference, Difference)
flipOffset (one, other, d1, d2, d3) = 
  let ds = [d1, d2, d3]
  in do
    xD <- findXDiff ds
    yD <- findYDiff ds
    zD <- findZDiff ds
    return (other, one, flipDifference d1, flipDifference d2, flipDifference d3)

mapOffsets :: [(Int, Int, Difference, Difference, Difference)] -> Offsets
mapOffsets = foldl' (\mp o@(one, other, xD, yD, zD) -> M.insertWith (++) one [(Offset other xD yD zD)]
                    $ case flipOffset o of
                        Nothing -> mp
                        Just (one2, other2, xD2, yD2, zD2) -> 
                          M.insertWith (++) one2 [(Offset other2 xD2 yD2 zD2)] mp) M.empty

mapOffsetsFromZero :: [(Int, Int, Difference, Difference, Difference)] -> Offsets
mapOffsetsFromZero = foldl' (\mp o@(one, other, xD, yD, zD) -> M.insertWith (++) one [(Offset other xD yD zD)] mp) M.empty

findOffsetFromZero :: Int -> Offsets -> Maybe [Offset]
findOffsetFromZero i offsets = findOffsets' S.empty [] i
  where findOffsets' :: S.Set Int -> [Offset] -> Int -> Maybe [Offset]
        findOffsets' seen acc curr = 
          if curr == 0 then Just (reverse acc)
          else case M.lookup curr offsets of
            Nothing -> Nothing
            Just os -> let unseen = filter (\o -> not $ S.member (offsetOther o) seen) os
                           newSeen = foldl' (\sn o -> S.insert (offsetOther o) sn) seen unseen
                       in case find isJust $ map (\o -> findOffsets' newSeen (o : acc) (offsetOther o)) unseen of
                            Nothing -> Nothing
                            Just path -> path

--part1  = (fmap (map (\(s1, s2, ds) -> (scannerId s1, scannerId s2, ds)) . findXMatches)) <$> input

findPathToZero :: Int -> Transforms -> Maybe [Transform]
findPathToZero i offsets = findTransforms' S.empty [] i
  where findTransforms' :: S.Set Int -> [Transform] -> Int -> Maybe [Transform]
        findTransforms' seen acc curr = 
          if curr == 0 then Just (reverse acc)
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

findPaths :: [(Int, Int, Difference, Difference, Difference)] -> [(Int, Maybe [Offset])]
findPaths pairOffsets = 
  let offsets = mapOffsets pairOffsets
      coords = S.toList $ foldl' (\st (i, j, _, _, _) -> S.insert i $ S.insert j st) S.empty pairOffsets
  in map (\i -> (i, findOffsetFromZero i offsets)) coords

noOffset :: Int -> Offset
noOffset i = (Offset i (Difference 0 1 X X) (Difference 0 1 Y Y) (Difference 0 1 Z Z))

findDestinationsFromZero :: Offsets -> [(Int, Offset)]
findDestinationsFromZero offsets = M.toList $ findDest' 0 (noOffset 0) M.empty
  where findDest' :: Int -> Offset -> M.Map Int Offset -> M.Map Int Offset
        findDest' i current seen = if M.size seen == M.size offsets
          then seen
          else case M.lookup i offsets of
                 Nothing -> seen
                 Just os -> 
                   let unseen = filter (\o -> not $ M.member (offsetOther o) seen) os
                       newSeen = foldl' (\sn o -> M.insert (offsetOther o) (fromJust (combineOffsets current o)) sn) seen unseen 
                   in foldl' (\sn o -> findDest' (offsetOther o) o sn) newSeen unseen

findPathsFromZero :: [(Int, Int, Difference, Difference, Difference)] -> [(Int, Offset)]
findPathsFromZero pairOffsets =
  let offsets = mapOffsetsFromZero pairOffsets
  in findDestinationsFromZero offsets


combineDifferences :: Difference -> Difference -> Difference
combineDifferences (Difference d1 f1 cOne1 _) (Difference d2 f2 _ cOther2) =
  Difference (d1 + (f1 * d2)) (f2 * f1) cOne1 cOther2
--  else Difference (d1 + d2) f1 cOne1 cOther2
  -- else if f2 == 1
  --      then Difference (d1 + d2) f1 cOne1 cOther2
  --      else Difference (d1 - d2) f1 cOne1 cOther2


findXInitDiff :: [Difference] -> Maybe Difference
findXInitDiff = L.find ((==X) . differenceCoordOne)

findYInitDiff :: [Difference] -> Maybe Difference
findYInitDiff = L.find ((==Y) . differenceCoordOne)

findZInitDiff :: [Difference] -> Maybe Difference
findZInitDiff = L.find ((==Z) . differenceCoordOne)

findPair :: Difference -> [Difference] -> Maybe Difference
findPair d ds = find (\other -> differenceCoordOther d == differenceCoordOne other) ds

combineOffsets :: Offset -> Offset -> Maybe Offset
combineOffsets (Offset _ dX1 dY1 dZ1) (Offset j dX2 dY2 dZ2) = 
  let ds = [dX2, dY2, dZ2]
  in do
    x <- findPair dX1 ds
    y <- findPair dY1 ds
    z <- findPair dZ1 ds
    let c1 = combineDifferences dX1 x
    let c2 = combineDifferences dY1 y 
    let c3 = combineDifferences dZ1 z
    return (Offset j c1 c2 c3)

combineAllOffsets :: [Offset] -> Maybe Offset
combineAllOffsets [] = Nothing
combineAllOffsets (first : rest) =
  foldl' (\acc o -> case acc of 
                      Nothing -> Nothing
                      Just off -> combineOffsets off o) (Just first) rest

dotVecs :: [Int] -> [Int] -> Int
dotVecs xs ys = sum $ zipWith (*) xs ys

matTimesVec :: [[Int]] -> [Int] -> [Int]
matTimesVec m v = map (\r -> dotVecs r v) m

matTimesMat :: [[Int]] -> [[Int]] -> [[Int]]
matTimesMat m1 m2 = map (matTimesVec m1) (L.transpose m2)

addVectors :: [Int] -> [Int] -> [Int]
addVectors = zipWith (+)

inv :: [[Int]] -> [[Int]]
inv = L.transpose

data Transform = Transform Int Int [[Int]] [Int] deriving Show

transformEnd :: Transform -> Int
transformEnd (Transform _ e _ _) = e

composeTransforms :: Transform -> Transform -> Transform
composeTransforms (Transform start _ m1 v1) (Transform _ end m2 v2) = 
  Transform start end (matTimesMat m1 m2) (addVectors (matTimesVec m1 v2) v1)

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

offsetsToZero :: [(Int, Int, Difference, Difference, Difference)] -> [(Int, Maybe Offset)]
offsetsToZero pairOffsets =
  let paths = findPaths pairOffsets
  in map (\(i, os) -> (i, os >>= combineAllOffsets)) paths

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

transformsToZero :: [(Int, Int, Difference, Difference, Difference)] -> M.Map Int (Maybe Transform)
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

part1 = (fmap (findTransformPaths . allMatches)) <$> input
