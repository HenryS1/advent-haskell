module Day16 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C

bits :: GenParser Char st String
bits = do
  c <- hexDigit
  return $ case c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"

allBits :: GenParser Char st String
allBits = concat <$> many1 bits

bitsToInt :: String -> Int
bitsToInt = foldl' (\acc b -> C.digitToInt b + 2 * acc) 0

bit :: GenParser Char st Char
bit = oneOf ['0', '1']

intBits :: Int -> GenParser Char st Int
intBits n = bitsToInt <$> count n bit 

data Packet = Literal Int Int | Operator Int Int [Packet] deriving Show

version :: GenParser Char st Int
version = intBits 3

middleChunk :: GenParser Char st String
middleChunk = char '1' *> count 4 bit

lastChunk :: GenParser Char st String
lastChunk = char '0' *> count 4 bit

literal :: GenParser Char st Packet
literal = do
  v <- version
  _ <- string "100"
  middle <- many middleChunk
  lst <- lastChunk
  let result = (Literal v (bitsToInt (concat middle ++ lst)))
  return $ result

packet :: GenParser Char st Packet
packet = try literal <|> operator

modeZero :: Int -> Int -> GenParser Char st Packet
modeZero v tId = do
  totalLength <- intBits 15
  packetBits <- count totalLength bit
  remaining <- getInput
  setInput packetBits
  subPackets <- many1 packet
  setInput remaining
  return (Operator v tId subPackets)

modeOne :: Int -> Int -> GenParser Char st Packet
modeOne v tId = do
  numSubPackets <- intBits 11
  subPackets <- count numSubPackets packet
  return (Operator v tId subPackets)

operator :: GenParser Char st Packet
operator = do
  v <- version
  typeId <- intBits 3
  lType <- bit
  case lType of
    '0' -> modeZero v typeId
    '1' -> modeOne v typeId

packetFromHex :: GenParser Char st Packet
packetFromHex = do
  bs <- allBits
  setInput bs
  packet

input :: IO (Either ParseError Packet)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day16.input"
  return $ parse packetFromHex "day16.input" (T.unpack fileContent)

versionSum :: Packet -> Int
versionSum (Literal v _) = v
versionSum (Operator v _ ps) = v + sum (map versionSum ps)

part1 :: IO (Either ParseError Int)
part1 = (fmap versionSum) <$> input

packetValue :: Packet -> Int
packetValue (Literal _ v) = v
packetValue (Operator _ 0 ps) = sum $ map packetValue ps
packetValue (Operator _ 1 ps) = product $ map packetValue ps
packetValue (Operator _ 2 ps) = minimum $ map packetValue ps
packetValue (Operator _ 3 ps) = maximum $ map packetValue ps
packetValue (Operator _ 5 [one, other]) = let v1 = packetValue one
                                              v2 = packetValue other
                                          in if v1 > v2 then 1 else 0
packetValue (Operator _ 6 [one, other]) = let v1 = packetValue one
                                              v2 = packetValue other
                                          in if v1 < v2 then 1 else 0
packetValue (Operator _ 7 [one, other]) = let v1 = packetValue one
                                              v2 = packetValue other
                                          in if v1 == v2 then 1 else 0

part2 :: IO (Either ParseError Int)
part2 = (fmap packetValue) <$> input
