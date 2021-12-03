module Day3 where

import Debug.Trace
import Data.Foldable
import Text.Parsec.Char
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import Data.Bits

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Binary = Binary { digits :: [Int] } deriving Show

binary :: GenParser Char st Binary
binary = Binary <$> many1 (read <$> (:[]) <$> digit)

numbers :: GenParser Char st [Binary]
numbers = many1 (binary <* endOfLine)

toInt :: Binary -> Int
toInt (Binary bs) = toInt' 0 bs
  where toInt' acc [] = acc
        toInt' acc (b : rest) = toInt' (acc * 2 + b) rest

bitCounts :: [Int] -> (Int, Int)
bitCounts = foldl' (\(one, zero) i -> if i .&. 1 == 0 then (one, zero + 1) else (one + 1, zero)) (0, 0)

mostCommonBit :: [Int] -> Int
mostCommonBit is = let (one, zero) = bitCounts is 
                   in if zero > one then 0 else 1

mostCommonBits :: [[Int]] -> [Int]
mostCommonBits bs = (map mostCommonBit) $ L.transpose bs

invert :: Binary -> Binary
invert (Binary bs) = Binary $ map (xor 1) bs

powerConsumption :: [Binary] -> Int
powerConsumption bs = let is = map digits bs
                          epsilon = Binary $ mostCommonBits is
                          gamma = invert epsilon
                      in toInt epsilon * toInt gamma

input :: IO (Either ParseError [Binary])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day3.input"
  return $ parse numbers "day3.input" (T.unpack fileContent)

part1 :: IO (Either ParseError Int)
part1 = (fmap powerConsumption) <$> input
  
data Zipper = Zipper { left :: [Int], right :: [Int]} deriving Show

zipRight :: Zipper -> Zipper
zipRight z@(Zipper _ []) = z
zipRight (Zipper l (r : rs)) = Zipper (r : l) rs

type BitIndicator = [Int] -> Int

filterByPredicate :: BitIndicator -> [Zipper] -> [Zipper]
filterByPredicate _ [] = []
filterByPredicate _ [z] = [z]
filterByPredicate bi zs@(z : _) = if null $ right z then zs 
  else let mcb = bi $ map (head . right) zs
       in filter (\zp -> case zp of
                     Zipper _ [] -> True
                     Zipper _ (b : _) -> b == mcb) zs

filterRepeatedly :: BitIndicator -> [Zipper] -> Maybe Zipper
filterRepeatedly _ [] = Nothing
filterRepeatedly _ [z] = Just z
filterRepeatedly bi zs = filterRepeatedly bi $ map zipRight $ filterByPredicate bi zs

filterByOxygen :: [Zipper] -> Maybe Zipper
filterByOxygen = filterRepeatedly mostCommonBit

filterByCO2 :: [Zipper] -> Maybe Zipper
filterByCO2 = filterRepeatedly (\is -> let (one, zero) = bitCounts is
                                       in if one < zero then 1 else 0)

binaryToZipper :: Binary -> Zipper
binaryToZipper (Binary bs) = Zipper [] bs

zipperToBinary :: Zipper -> Binary
zipperToBinary (Zipper ls rs) = Binary ((reverse ls) ++ rs)

findOxygenRating :: [Binary] -> Maybe Int
findOxygenRating bs = let zs = map binaryToZipper bs
                          oxygenZip = filterByOxygen zs
                      in toInt <$> zipperToBinary <$> oxygenZip

findCO2Rating :: [Binary] -> Maybe Int
findCO2Rating bs = let zs = map binaryToZipper bs
                       co2Zip = filterByCO2 zs
                   in toInt <$> zipperToBinary <$> co2Zip

lifeSupportRating :: [Binary] -> Maybe Int
lifeSupportRating bs = do
  oxygenRating <- findOxygenRating bs
  co2Rating <- findCO2Rating bs
  return $ oxygenRating * co2Rating

part2 :: IO (Either ParseError (Maybe Int))
part2 = (fmap lifeSupportRating) <$> input

  
