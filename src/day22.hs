module Day22 where

import Debug.Trace
import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

int :: GenParser Char st Int
int = (char '-' *> ((*(-1)) <$> positiveInt)) <|> positiveInt

data Bounds = Bounds Int Int deriving Show

data Switch = On | Off | NoOp deriving (Eq, Show)

data Cuboid = Cuboid Switch Bounds Bounds Bounds deriving Show

parseBounds :: GenParser Char st Bounds
parseBounds = do
  bottom <- int
  _ <- string ".."
  top <- int
  return (Bounds bottom top)

parseSwitch :: GenParser Char st Switch
parseSwitch = try (string "on" *> (pure On)) <|> (string "off" *> (pure Off))

parseCuboid :: GenParser Char st Cuboid
parseCuboid = do
  sw <- parseSwitch
  _ <- string " x="
  x <- parseBounds
  _ <- string ",y="
  y <- parseBounds
  _ <- string ",z="
  z <- parseBounds
  return (Cuboid sw x y z)

parseCuboids :: GenParser Char st [Cuboid]
parseCuboids = many1 (parseCuboid <* endOfLine)

input :: IO (Either ParseError [Cuboid])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day22.input"
  return $ parse parseCuboids "day22.input" (T.unpack fileContent)

data Tree = Tree { 
  outstanding :: Switch,
  xMin :: Int,
  xMax :: Int,
  yMin :: Int,
  yMax :: Int,
  zMin :: Int,
  zMax :: Int,
  onCount :: Int,
  xMnyMnzMn :: Maybe Tree,
  xMnyMxzMn :: Maybe Tree,
  xMxyMxzMn :: Maybe Tree,
  xMxyMnzMn :: Maybe Tree,
  xMnyMnzMx :: Maybe Tree,
  xMnyMxzMx :: Maybe Tree,
  xMxyMxzMx :: Maybe Tree,
  xMxyMnzMx :: Maybe Tree
} deriving Show

makeTree :: Switch -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Tree
makeTree sw xMn xMx yMn yMx zMn zMx on = 
  Tree sw xMn xMx yMn yMx zMn zMx on Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

updateDefaults :: Switch -> Tree -> Tree
updateDefaults sw tree = update sw (xMin tree) (xMax tree) (yMin tree) (yMax tree) (zMin tree) (zMax tree) tree

combine :: Switch -> Switch -> Switch
combine On Off = NoOp
combine On On = On
combine Off Off = Off
combine Off On = NoOp
combine NoOp other = other
combine other NoOp = other

updatedOnCount :: Switch -> Int -> Int -> Int -> Int -> Int
updatedOnCount Off _ _ _ _ = 0
updatedOnCount On xDiff yDiff zDiff _ = --trace ("xDiff " ++ show xDiff ++ " yDiff " ++ show yDiff ++ " zDiff " ++ show zDiff) $ 
  xDiff * yDiff * zDiff
updatedOnCount NoOp _ _ _ oldOnCount = oldOnCount

update :: Switch -> Int -> Int -> Int -> Int -> Int -> Int -> Tree -> Tree
update sw xMn xMx yMn yMx zMn zMx tree =
--  trace ("update " ++ show sw ++ " xMn " ++ show xMn ++ " xMx " ++ show xMx ++ " yMn " ++ show yMn ++ " yMx " ++ show yMx ++ " zMn " ++ show zMn ++ " zMx " ++ show zMx) $
  let xDiff = xMx - xMn
      yDiff = yMx - yMn
      zDiff = zMx - zMn
  in if xDiff == 0 || yDiff == 0 || zDiff == 0 || sw == NoOp
     then tree
     else let combined = combine sw (outstanding tree)
          in if xMin tree == xMn
                && xMax tree == xMx
                && yMin tree == yMn
                && yMax tree == yMx
                && zMin tree == zMn
                && zMax tree == zMx
             then (tree { outstanding = combined, onCount = updatedOnCount sw xDiff yDiff zDiff (onCount tree)}) 
             else let xMid = (xMin tree + xMax tree) `div` 2
                      yMid = (yMin tree + yMax tree) `div` 2
                      zMid = (zMin tree + zMax tree) `div` 2
                      defaultxMnyMnzMn = fromMaybe (makeTree NoOp (xMin tree) xMid (yMin tree) yMid (zMin tree) zMid 0) (xMnyMnzMn tree)
                      defaultxMnyMxzMn = fromMaybe (makeTree NoOp (xMin tree) xMid yMid (yMax tree) (zMin tree) zMid 0) (xMnyMxzMn tree)
                      defaultxMxyMxzMn = fromMaybe (makeTree NoOp xMid (xMax tree) yMid (yMax tree) (zMin tree) zMid 0) (xMxyMxzMn tree)
                      defaultxMxyMnzMn = fromMaybe (makeTree NoOp xMid (xMax tree) (yMin tree) yMid (zMin tree) zMid 0) (xMxyMnzMn tree)
                      
                      defaultxMnyMnzMx = fromMaybe (makeTree NoOp (xMin tree) xMid (yMin tree) yMid zMid (zMax tree) 0) (xMnyMnzMx tree)
                      defaultxMnyMxzMx = fromMaybe (makeTree NoOp (xMin tree) xMid yMid (yMax tree) zMid (zMax tree) 0) (xMnyMxzMx tree)
                      defaultxMxyMxzMx = fromMaybe (makeTree NoOp xMid (xMax tree) yMid (yMax tree) zMid (zMax tree) 0) (xMxyMxzMx tree)
                      defaultxMxyMnzMx = fromMaybe (makeTree NoOp xMid (xMax tree) (yMin tree) yMid zMid (zMax tree) 0) (xMxyMnzMx tree)
                      newxMnyMnzMn = if xMn < xMid && yMn < yMid && zMn < zMid
                        then update sw xMn (xMid `min` xMx) yMn (yMid `min` yMx) zMn (zMid `min` zMx) $ updateDefaults (outstanding tree) defaultxMnyMnzMn
                        else updateDefaults (outstanding tree) defaultxMnyMnzMn
                      newxMnyMxzMn = if xMn < xMid && yMx >= yMid && zMn < zMid
                        then update sw xMn (xMid `min` xMx) (yMid `max` yMn) yMx zMn (zMid `min` zMx) $ updateDefaults (outstanding tree) defaultxMnyMxzMn
                        else updateDefaults (outstanding tree) defaultxMnyMxzMn
                      newxMxyMxzMn = if xMx >= xMid && yMx >= yMid && zMn < zMid
                        then update sw (xMid `max` xMn) xMx (yMid `max` yMn) yMx zMn (zMid `min` zMx) $ updateDefaults (outstanding tree) defaultxMxyMxzMn
                        else updateDefaults (outstanding tree) defaultxMxyMxzMn
                      newxMxyMnzMn = if xMx >= xMid && yMn < yMid && zMn < zMid
                        then update sw (xMid `max` xMn) xMx yMn (yMid `min` yMx) zMn (zMid `min` zMx) $ updateDefaults (outstanding tree) defaultxMxyMnzMn
                        else updateDefaults (outstanding tree) defaultxMxyMnzMn
                      
                      newxMnyMnzMx = if xMn < xMid && yMn < yMid && zMx >= zMid
                        then update sw xMn (xMid `min` xMx) yMn (yMid `min` yMx) (zMid `max` zMn) zMx $ updateDefaults (outstanding tree) defaultxMnyMnzMx
                        else updateDefaults (outstanding tree) defaultxMnyMnzMx
                      newxMnyMxzMx = if xMn < xMid && yMx >= yMid && zMx >= zMid
                        then update sw xMn (xMid `min` xMx) (yMid `max` yMn) yMx (zMid `max` zMn) zMx $ updateDefaults (outstanding tree) defaultxMnyMxzMx
                        else updateDefaults (outstanding tree) defaultxMnyMxzMx
                      newxMxyMxzMx = if xMx >= xMid && yMx >= yMid && zMx >= zMid
                        then update sw (xMid `max` xMn) xMx (yMid `max` yMn) yMx (zMid `max` zMn) zMx $ updateDefaults (outstanding tree) defaultxMxyMxzMx
                        else updateDefaults (outstanding tree) defaultxMxyMxzMx
                      newxMxyMnzMx = if xMx >= xMid && yMn < yMid && zMx >= zMid
                        then update sw (xMid `max` xMn) xMx yMn (yMid `min` yMx) (zMid `max` zMn) zMx $ updateDefaults (outstanding tree) defaultxMxyMnzMx
                        else updateDefaults (outstanding tree) defaultxMxyMnzMx
                  in --trace (show (onCount newxMnyMnzMn) ++ " " ++ show (onCount newxMnyMxzMn) ++ " " ++ show (onCount newxMxyMxzMn) ++ " " ++ show (onCount newxMxyMnzMn) ++ " " ++ show (onCount newxMnyMnzMx) ++ " " ++ show (onCount newxMnyMxzMx) ++ " " ++ show (onCount newxMxyMxzMx) ++ " " ++ show (onCount newxMxyMnzMx)) $
    (tree 
                      {
                        onCount = (onCount newxMnyMnzMn) + (onCount newxMnyMxzMn) + (onCount newxMxyMxzMn) + (onCount newxMxyMnzMn) 
                                  + (onCount newxMnyMnzMx) + (onCount newxMnyMxzMx) + (onCount newxMxyMxzMx) + (onCount newxMxyMnzMx),
                        xMnyMnzMn = Just newxMnyMnzMn,
                        xMnyMxzMn = Just newxMnyMxzMn,
                        xMxyMxzMn = Just newxMxyMxzMn,
                        xMxyMnzMn = Just newxMxyMnzMn,
                        
                        xMnyMnzMx = Just newxMnyMnzMx,
                        xMnyMxzMx = Just newxMnyMxzMx,
                        xMxyMxzMx = Just newxMxyMxzMx,
                        xMxyMnzMx = Just newxMxyMnzMx,
                        outstanding = NoOp
                      }
                     )

applyUpdate :: Tree -> Cuboid -> Tree
applyUpdate tr cb@(Cuboid sw (Bounds xMn xMx) (Bounds yMn yMx) (Bounds zMn zMx)) = 
--  trace ("CUBOID " ++ show cb) $
  update sw xMn (xMx + 1)  yMn (yMx + 1) zMn (zMx + 1) tr

enclosingBound :: Bounds -> Bounds -> Bounds
enclosingBound (Bounds mn1 mx1) (Bounds mn2 mx2) = Bounds (mn1 `min` mn2) (mx1 `max` mx2)

enclosingCuboids :: Cuboid -> Cuboid -> Cuboid
enclosingCuboids (Cuboid sw x1 y1 z1) (Cuboid _ x2 y2 z2) = 
  Cuboid sw (enclosingBound x1 x2) (enclosingBound y1 y2) (enclosingBound z1 z2)

initialTree :: [Cuboid] -> Tree
initialTree cs = let (Cuboid _ (Bounds xMn xMx) (Bounds yMn yMx) (Bounds zMn zMx)) = foldr1 enclosingCuboids cs
                 in makeTree NoOp xMn (xMx + 1) yMn (yMx + 1) zMn (zMx + 1) 0

normaliseBounds :: Bounds -> Maybe Bounds
normaliseBounds (Bounds mn mx) = if (mn <= -50 && mx >= -50) || (mn <= 50 && mx >= 50) || (mn >= -50 && mx <= 50)
  then Just (Bounds ((mn `max` (-50)) `min` 50) ((mx `min` 50) `max` (-50)))
  else Nothing

normaliseCuboid :: Cuboid -> Maybe Cuboid
normaliseCuboid (Cuboid sw x y z) = do
  xN <- normaliseBounds x
  yN <- normaliseBounds y
  zN <- normaliseBounds z
  return $ Cuboid sw xN yN zN

--answer1 :: [Cuboid] -> Int
answer1 cs = let initCuboids = catMaybes $ map normaliseCuboid cs
                 tr = initialTree initCuboids
             in --trace ("TREE " ++ show tr) $
                onCount $ foldl' applyUpdate tr initCuboids 

type Grid = S.Set (Int, Int, Int)

updateGrid :: Grid -> Cuboid -> Grid
updateGrid gr (Cuboid sw (Bounds xMn xMx) (Bounds yMn yMx) (Bounds zMn zMx)) =
  case sw of 
    On -> foldl' (\g c -> S.insert c g) gr [(x, y, z)| x <- [xMn..xMx], y <- [yMn..yMx], z <- [zMn..zMx]]
    Off -> foldl' (\g c -> S.delete c g) gr [(x, y, z)| x <- [xMn..xMx], y <- [yMn..yMx], z <- [zMn..zMx]]
    NoOp -> gr

answer :: [Cuboid] -> Int
answer cs = let initCuboids = catMaybes $ map normaliseCuboid cs
                finalGrid = foldl' updateGrid S.empty initCuboids
            in S.size finalGrid

part1 = (fmap answer) <$> input
