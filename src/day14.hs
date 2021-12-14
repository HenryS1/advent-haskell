module Day14 where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.Set as S

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

parseRule :: GenParser Char st (String, Char)
parseRule = do
  hd <- many1 letter
  _ <- string " -> "
  replacement <- letter
  return (hd, replacement)

parseRules :: GenParser Char st [(String, Char)]
parseRules = many1 (parseRule <* endOfLine)

data Input = Input String (M.Map String Char) deriving Show

parseInput :: GenParser Char st Input
parseInput = do
  polymer <- (many1 letter <* endOfLine)
  _ <- endOfLine
  rules <- parseRules
  return (Input polymer (M.fromList rules))

input :: IO (Either ParseError Input)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day14.input"
  return $ parse parseInput "day14.input" (T.unpack fileContent)

type Polymer = String
type Rules = M.Map String Char
type PairCounts = M.Map String Integer
type Pair = String
type Cache = M.Map (String, Int) PairCounts

countPairs :: Polymer -> PairCounts
countPairs [] = M.empty
countPairs [_] = M.empty
countPairs (c : tl@(d : _)) = M.insertWith (+) [c, d] 1 $ countPairs tl


applySubstitutions :: PairCounts -> Rules -> PairCounts
applySubstitutions pcs rs = M.foldrWithKey (\pr@[a, b] cnt acc -> 
                                             case M.lookup pr rs of
                                               Nothing -> M.insertWith (+) pr 1 acc
                                               Just repl ->
                                                 M.insertWith (+) [a,repl] cnt $ 
                                                 M.insertWith (+) [repl, b] cnt acc) M.empty pcs

type Element = Char
type First = Char
type Last = Char

initElementCount :: Char -> PairCounts -> Integer
initElementCount c = M.foldrWithKey (\[a,_] cnt acc -> if a == c then acc + cnt else acc) 0

lstElementCount :: Char -> PairCounts -> Integer
lstElementCount c = M.foldrWithKey (\[_, b] cnt acc -> if b == c then acc + cnt else acc) 0

elementCount :: First -> Last -> PairCounts -> Char -> Integer
elementCount f l pcs c = let initCnt = initElementCount c pcs
                             lstCnt = lstElementCount c pcs
                         in (initCnt `max` lstCnt) + if f == c && l == c then 1 else 0

applyRepeatedly :: Rules -> Int -> PairCounts -> PairCounts
applyRepeatedly rs i pcs = if i == 0 then pcs else applyRepeatedly rs (i - 1) (applySubstitutions pcs rs)

applyToInput :: Int -> Input -> PairCounts
applyToInput i (Input p rs) = applyRepeatedly rs i (countPairs p)

elements :: PairCounts -> [Char]
elements pcs = S.toList $ S.fromList $ concat $ M.keys pcs

answer :: Int -> Input -> Integer
answer i inp@(Input p _) = let pcs = applyToInput i inp
                               f = head p
                               l = last p
                               elCounts = map (elementCount f l pcs) (elements pcs)
                           in maximum elCounts - minimum elCounts

part1 :: IO (Either ParseError Integer)
part1 = (fmap (answer 10)) <$> input

part2 :: IO (Either ParseError Integer)
part2 = (fmap (answer 40)) <$> input
