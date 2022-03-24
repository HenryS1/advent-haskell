module Day22 where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe

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

switch :: Cuboid -> Switch 
switch (Cuboid sw _ _ _) = sw

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

findIntersection :: Cuboid -> Cuboid -> [Cuboid]
findIntersection c1 c2 = let inter = intersectCuboids c1 c2
                             in case inter of 
                                  Nothing -> [c2]
                                  Just i -> splitCuboid c2 i

updateCuboids :: Cuboid -> [Cuboid] -> [Cuboid]
updateCuboids c@(Cuboid On _ _ _) cs = c : (cs >>= (findIntersection c))
updateCuboids c@(Cuboid Off _ _ _) cs = cs >>= (findIntersection c)

intersectAll :: [Cuboid] -> [Cuboid]
intersectAll [] = []
intersectAll (c : cs) = let rest = (intersectAll cs)
                           in case (rest, switch c) of 
                                ([], On) -> [c]
                                ([], Off) -> []
                                (inters, _) -> updateCuboids c inters

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

volume :: Cuboid -> Integer
volume (Cuboid _ (Bounds xMn xMx) (Bounds yMn yMx) (Bounds zMn zMx)) = 
  fromIntegral $ (xMx - xMn + 1) * (yMx - yMn + 1) * (zMx - zMn + 1)

volumeOn :: [Cuboid] -> Integer
volumeOn = sum . map volume . intersectAll . reverse

initializationArea :: Cuboid 
initializationArea = Cuboid On (Bounds (-50) 50) (Bounds (-50) 50) (Bounds (-50) 50)

part1 :: IO (Either ParseError Integer)
part1 = fmap (volumeOn . catMaybes . map (intersectCuboids initializationArea)) 
  <$> input

part2 :: IO (Either ParseError Integer)
part2 = fmap volumeOn <$> input
