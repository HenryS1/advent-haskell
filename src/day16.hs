module Day16 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

bits :: GenParser Char st String
bits = (char '0' >> pure "0000")
    <|> (char '1' >> pure "0001")
    <|> (char '2' >> pure "0010")
    <|> (char '3' >> pure "0011")
    <|> (char '4' >> pure "0100")
    <|> (char '5' >> pure "0101")
    <|> (char '6' >> pure "0110")
    <|> (char '7' >> pure "0111")
    <|> (char '8' >> pure "1000")
    <|> (char '9' >> pure "1001")
    <|> (char 'A' >> pure "1010")
    <|> (char 'B' >> pure "1011")
    <|> (char 'C' >> pure "1100")
    <|> (char 'D' >> pure "1101")
    <|> (char 'E' >> pure "1110")
    <|> (char 'F' >> pure "1111")

allBits :: GenParser Char st String
allBits = concat <$> many1 bits

bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\acc b -> b + 2 * acc) 0

bit :: GenParser Char st Int
bit = (char '0' >> pure 0) <|> (char '1' >> pure 1)

intBits :: Int -> GenParser Char st Int
intBits n = bitsToInt <$> count n bit 

data Operator = Sum [Packet]
  | Product [Packet]
  | Minimum [Packet]
  | Maximum [Packet]
  | Gt Packet Packet
  | Lt Packet Packet
  | Equal Packet Packet deriving Show

data Packet = Literal Int Int 
  | Operation Int Operator deriving Show

version :: GenParser Char st Int
version = intBits 3

middleChunk :: GenParser Char st [Int]
middleChunk = char '1' *> count 4 bit

lastChunk :: GenParser Char st [Int]
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
packet = try literal <|> operation

parseOperator :: Int -> [Packet] -> GenParser Char st Operator
parseOperator tId ps = (guard (tId == 0) >> pure (Sum ps))
  <|> (guard (tId == 1) >> 
       do _ : _ <- pure ps
          return (Product ps)
            <|> fail "Expected at least one packet in ")
  <|> (guard (tId == 2) >> pure (Minimum ps))
  <|> (guard (tId == 3) >> pure (Maximum ps))
  <|> (guard (tId == 5) >>
        do [one, other] <- pure ps
           return (Gt one other)
            <|> fail "Invalid number of arguments for less than operator")
  <|> (guard (tId == 6) >>
        do [one, other] <- pure ps
           return (Lt one other) 
             <|> fail "Invalid number of arguments for greater than operator")
  <|> (guard (tId == 7) >>
        do [one, other] <- pure ps
           return (Equal one other) 
             <|> fail "Invalid number of arguments for equal operator")
  <|> fail ("Unknown operator id " ++ show tId)

modeZero :: Int -> Int -> GenParser Char st Packet
modeZero v tId = do
  totalLength <- intBits 15
  packetBits <- count totalLength anyChar
  remaining <- getInput
  setInput packetBits
  subPackets <- many1 packet
  setInput remaining
  operator <- parseOperator tId subPackets
  return (Operation v operator)

modeOne :: Int -> Int -> GenParser Char st Packet
modeOne v tId = do
  numSubPackets <- intBits 11
  subPackets <- count numSubPackets packet
  operator <- parseOperator tId subPackets
  return (Operation v operator)

operation :: GenParser Char st Packet
operation = do
  v <- version
  typeId <- intBits 3
  (char '0' >> modeZero v typeId) <|> (char '1' >> modeOne v typeId)

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
versionSum (Operation v operator) = v + operatorVersionSum operator
  where operatorVersionSum (Sum ps) = sum $ map versionSum ps
        operatorVersionSum (Product ps) = sum $ map versionSum ps
        operatorVersionSum (Minimum ps) = sum $ map versionSum ps
        operatorVersionSum (Maximum ps) = sum $ map versionSum ps
        operatorVersionSum (Gt one other) = versionSum one + versionSum other
        operatorVersionSum (Lt one other) = versionSum one + versionSum other
        operatorVersionSum (Equal one other) = versionSum one + versionSum other

part1 :: IO (Either ParseError Int)
part1 = (fmap versionSum) <$> input

packetValue :: Packet -> Int
packetValue (Literal _ v) = v
packetValue (Operation _ (Sum ps)) = sum $ map packetValue ps
packetValue (Operation _ (Product ps)) = product $ map packetValue ps
packetValue (Operation _ (Minimum ps)) = minimum $ map packetValue ps
packetValue (Operation _ (Maximum ps)) = maximum $ map packetValue ps
packetValue (Operation _ (Gt one other)) = let v1 = packetValue one
                                               v2 = packetValue other
                                           in if v1 > v2 then 1 else 0
packetValue (Operation _ (Lt one other)) = let v1 = packetValue one
                                               v2 = packetValue other
                                           in if v1 < v2 then 1 else 0
packetValue (Operation _ (Equal one other)) = let v1 = packetValue one
                                                  v2 = packetValue other
                                              in if v1 == v2 then 1 else 0

part2 :: IO (Either ParseError Int)
part2 = (fmap packetValue) <$> input
