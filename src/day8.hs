module Day8 where

import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.List as L

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

positions :: GenParser Char st [Int]
positions = sepBy1 positiveInt (char ',')

data Digit = Digit String deriving Show

data Configuration = Configuration [String] [String] deriving Show

parseDigit :: GenParser Char st String
parseDigit = many1 (noneOf [' ', '\n', '\r', '|'])

digitGroup :: GenParser Char st [String]
digitGroup = do 
  first <- parseDigit
  rest <- many (try (char ' ' *> parseDigit))
  return (first : rest)

configuration :: GenParser Char st Configuration
configuration = do
  digits <- digitGroup
  _ <- string " | "
  display <- digitGroup
  return (Configuration (map L.sort digits) (map L.sort display))

configurationLines :: GenParser Char st [Configuration]
configurationLines = many1 (configuration <* endOfLine)

input :: IO (Either ParseError [Configuration])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day8.input"
  return $ parse configurationLines "day8.input" (T.unpack fileContent)

countKnownNumbers :: Configuration -> Int
countKnownNumbers (Configuration _ display) = length $ filter (((flip elem) [2, 3, 4, 7]) . length) display

part1 :: IO (Either ParseError Int)
part1 = (fmap ((foldl' (+) 0) . (map countKnownNumbers))) <$> input

substitute :: M.Map Char Char -> String -> String
substitute mp s = catMaybes $ map ((flip M.lookup) mp) s

findOne :: [String] -> Maybe String
findOne = find ((==2) . length)

findFour :: [String] -> Maybe String
findFour = find ((==4) . length)

findSeven :: [String] -> Maybe String
findSeven = find ((==3) . length)

findEight :: [String] -> Maybe String
findEight = find ((==7) . length)

allIn :: String -> String -> Bool
allIn one other = and ((map ((flip elem) other)) one)

overlapCount :: String -> String -> Int
overlapCount one other = length $ filter ((flip elem) other) one

findThree :: String -> [String] -> Maybe String
findThree one = find (\s -> length s == 5 && (allIn one s))

findTwo :: String -> [String] -> Maybe String
findTwo nine = find (\s -> length s == 5 && (overlapCount nine s == 4))

findFive :: String -> String -> [String] -> Maybe String
findFive one four = find (\s -> length s == 5 && (overlapCount one s == 1) && overlapCount four s == 3)

findSix :: String -> [String] -> Maybe String
findSix one = find (\s -> length s == 6 && (overlapCount one s == 1))

findZero :: String -> String -> [String] -> Maybe String
findZero one three = find (\s -> length s == 6 && (overlapCount one s == 2) && (overlapCount three s == 4))

findNine :: String -> [String] -> Maybe String
findNine three = find (\s -> length s == 6 && overlapCount three s == 5)

findDigits :: [String] -> Maybe (M.Map String Int)
findDigits ds = do
  one <- findOne ds
  four <- findFour ds
  seven <- findSeven ds
  eight <- findEight ds
  three <- findThree one ds
  five <- findFive one four ds
  six <- findSix one ds
  zero <- findZero one three ds
  nine <- findNine three ds
  two <- findTwo nine ds
  return $ M.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), 
                       (four, 4), (five, 5), (six, 6), (seven, 7),
                       (eight, 8), (nine, 9)]


listToInt :: [Int] -> Int
listToInt ds = listToInt' 0 ds
  where listToInt' acc [] = acc
        listToInt' acc (d : rest) = listToInt' (10 * acc + d) rest

displayValue :: M.Map String Int -> [String] -> Maybe [Int]
displayValue _ [] = Just []
displayValue mp (d : rest) = case M.lookup d mp of
  Nothing -> Nothing
  Just n -> (n:) <$> (displayValue mp rest)

configurationValue :: Configuration -> Maybe Int
configurationValue (Configuration digits display) = do
  digitMapping <- findDigits digits
  asList <- displayValue digitMapping display
  return (listToInt asList)

addConfigurationValues :: [Maybe Int] -> Maybe Int
addConfigurationValues = foldl' (\acc i -> acc >>= (\j -> fmap (+j) i)) (Just 0)
  
part2 :: IO (Either ParseError (Maybe Int))
part2 = (fmap (addConfigurationValues . (map configurationValue))) <$> input

