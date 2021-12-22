module Day22 where

import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
}

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

updatedOnCount :: Switch -> Int -> Int -> Int -> Int
updatedOnCount Off _ _ _ = 0
updatedOnCount On xDiff yDiff zDiff = xDiff * yDiff * zDiff

update :: Switch -> Int -> Int -> Int -> Int -> Int -> Int -> Tree -> Tree
update sw xMn xMx yMn yMx zMn zMx tree =
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
             then (tree { outstanding = combined, onCount = updatedOnCount sw xDiff yDiff zDiff }) 
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
                  in (tree 
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
                        xMxyMnzMx = Just newxMxyMnzMx
                      }
                     )
