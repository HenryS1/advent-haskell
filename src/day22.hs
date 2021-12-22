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

data Bounds = Bounds Int Int deriving (Eq, Show)

data Switch = On | Off deriving (Eq, Show)

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


intersectBounds :: Bounds -> Bounds -> Maybe Bounds
intersectBounds b1@(Bounds mn1 mx1) b2@(Bounds mn2 mx2) = 
  if mn1 > mn2
  then intersectBounds b2 b1
  else if mx1 >= mx2 
       then Just b2
       else if mx1 < mn2 
            then Nothing
            else Just (Bounds mn2 mx1)



intersectCuboids :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCuboids (Cuboid _ x1 y1 z1) (Cuboid sw x2 y2 z2) = do
  x <- intersectBounds x1 x2
  y <- intersectBounds y1 y2
  z <- intersectBounds z1 z2
  return (Cuboid sw x y z)

type Intersection = Cuboid

sameBlock :: Cuboid -> Cuboid -> Bool
sameBlock (Cuboid _ x1 y1 z1) (Cuboid _ x2 y2 z2) =
  x1 == x2 && y1 == y2 && z1 == z2

boundsEmpty :: Bounds -> Bool
boundsEmpty (Bounds mn mx) = mx < mn

cuboidEmpty :: Cuboid -> Bool
cuboidEmpty (Cuboid _ x y z) = boundsEmpty x || boundsEmpty y || boundsEmpty z

splitCuboid :: Cuboid -> Intersection -> [Cuboid]
splitCuboid c1@(Cuboid sw (Bounds xMn1 xMx1) (Bounds yMn1 yMx1) (Bounds zMn1 zMx1)) c2@(Cuboid _ (Bounds xMn2 xMx2) (Bounds yMn2 yMx2) (Bounds zMn2 zMx2)) = if sameBlock c1 c2 
  then []
  else filter (not . cuboidEmpty)
  [Cuboid sw (Bounds xMn1 (xMn2 - 1)) (Bounds yMn1 yMx1) (Bounds zMn1 zMx1),
   Cuboid sw (Bounds (xMx2 + 1) xMx1) (Bounds yMn1 yMx1) (Bounds zMn1 zMx1),
   Cuboid sw (Bounds xMn2 xMx2) (Bounds yMn1 (yMn2 - 1)) (Bounds zMn1 zMx1),
   Cuboid sw (Bounds xMn2 xMx2) (Bounds (yMx2 + 1) yMx1) (Bounds zMn1 zMx1),
   Cuboid sw (Bounds xMn2 xMx2) (Bounds yMn2 yMx2) (Bounds zMn1 (zMn2 - 1)),
   Cuboid sw (Bounds xMn2 xMx2) (Bounds yMn2 yMx2) (Bounds (zMx2 + 1) zMx1)]

combineCuboids :: Cuboid -> Cuboid -> ([Cuboid], [Cuboid])
combineCuboids c1 c2 = 
  let intersection = intersectCuboids c1 c2
  in case intersection of
    Nothing -> ([c1], [c2])
    Just intersct ->
      let old = splitCuboid c1 intersct
          new = splitCuboid c2 intersct
      in (old, intersct : new)

isOff :: Cuboid -> Bool
isOff (Cuboid On _ _ _) = False
isOff (Cuboid Off _ _ _) = True


addNewCuboid :: Cuboid -> [Cuboid] -> [Cuboid]
addNewCuboid c [] = [c]
addNewCuboid c1 cs = 
  let (old, new) = unzip $ map (combineCuboids c1) cs
  in filter (not . isOff) $ (concat old) ++ concat new
--  in filter (not . isOff) $ (old ++ new)

volume :: Cuboid -> Int
volume (Cuboid _ (Bounds xMn xMx) (Bounds yMn yMx) (Bounds zMn zMx)) = 
  (xMx - xMn + 1) * (yMx - yMn + 1) * (zMx - zMn + 1)

countOn :: [Cuboid] -> Int
countOn = sum . map volume . filter (not . isOff)

combineAll :: [Cuboid] -> [Cuboid]
combineAll cuboids = combine' cuboids []
  where combine' [] acc = acc
        combine' (c : cs) acc = combine' cs (addNewCuboid c acc)

--answer :: [Cuboid] -> [Cuboid]
answer = length . combineAll

part2 = (fmap answer) <$> input

--main = part1 >>= (putStrLn . show)
