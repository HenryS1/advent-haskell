module Day3 where

import Data.Foldable
import Text.Parsec.Char
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Bits
import qualified Data.Char as C

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Binary = Binary { len :: Int, n :: Int } deriving Show

bitsToInt :: [Int] -> Int
bitsToInt bs = toInt' 0 bs
  where toInt' i [] = i
        toInt' i (b : rest) = toInt' (2 * i + b) rest

binary :: GenParser Char st Binary
binary = do 
  s <- many1 digit
  return (Binary (length s) (bitsToInt (map C.digitToInt s)))

numbers :: GenParser Char st [Binary]
numbers = many1 (binary <* endOfLine)

type BitSelector = [Binary] -> Int -> Int

type Predicate = Int -> Int -> Bool

bitCountSelector :: Predicate -> BitSelector
bitCountSelector p bs index = 
  let (ones, zeros) = foldl' (\(one, zero) (Binary _ v) -> 
                                if v .&. (shiftL 1 index) == 0 
                                then (one, zero + 1) else (one + 1, zero)) (0, 0) bs
  in if p ones zeros then 1 else 0

oneBiasedMostCommonBit :: BitSelector
oneBiasedMostCommonBit = bitCountSelector (>=)

zeroBiasedLeastCommonBit :: BitSelector
zeroBiasedLeastCommonBit = bitCountSelector (<)

oneBiasedMostCommonBits :: [Binary] -> [Int]
oneBiasedMostCommonBits [] = []
oneBiasedMostCommonBits bs@((Binary ln _) : _) = 
  map (oneBiasedMostCommonBit bs) [ln - 1, ln - 2 .. 0]

invert :: Binary -> Binary
invert (Binary ln bs) = Binary ln (bs `xor` (shiftL 1 ln - 1))

powerConsumption :: [Binary] -> Int
powerConsumption [] = 0
powerConsumption bs@(Binary ln _ : _) = 
  let epsilon = Binary ln (bitsToInt $ oneBiasedMostCommonBits bs)
      gamma = invert epsilon
  in (n epsilon) * (n gamma)

input :: IO (Either ParseError [Binary])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day3.input"
  return $ parse numbers "day3.input" (T.unpack fileContent)

part1 :: IO (Either ParseError Int)
part1 = (fmap powerConsumption) <$> input

filterByPredicate :: BitSelector -> [Binary] -> Int -> [Binary]
filterByPredicate _ [] _ = []
filterByPredicate bi bs i = 
  let bitChoice = bi bs i
  in filter (\(Binary _ v) -> v .&. (shiftL 1 i) == (shiftL bitChoice i)) bs 

filterRepeatedly :: BitSelector -> [Binary] -> Int -> Maybe Binary
filterRepeatedly _ [] _ = Nothing
filterRepeatedly _ [b] _ = Just b
filterRepeatedly bi bs@(Binary _ _ : _) i = 
  if i < 0 
  then Nothing
  else filterRepeatedly bi (filterByPredicate bi bs i) (i - 1)

filterByOxygen :: [Binary] -> Maybe Binary
filterByOxygen [] = Nothing
filterByOxygen bs@(Binary l _ : _) = filterRepeatedly oneBiasedMostCommonBit bs (l - 1)

filterByCO2 :: [Binary] -> Maybe Binary
filterByCO2 [] = Nothing
filterByCO2 bs@(Binary l _ : _) = filterRepeatedly zeroBiasedLeastCommonBit bs (l - 1)

findOxygenRating :: [Binary] -> Maybe Int
findOxygenRating bs = n <$> filterByOxygen bs

findCO2Rating :: [Binary] -> Maybe Int
findCO2Rating bs = n <$> filterByCO2 bs

lifeSupportRating :: [Binary] -> Maybe Int
lifeSupportRating bs = do
  oxygenRating <- findOxygenRating bs
  co2Rating <- findCO2Rating bs
  return $ oxygenRating * co2Rating

part2 :: IO (Either ParseError (Maybe Int))
part2 = (fmap lifeSupportRating) <$> input
