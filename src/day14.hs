module Day14 where

import Data.Foldable
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

substitutions :: Polymer -> Rules -> Polymer
substitutions [] _ = []
substitutions lst@[_] _ = lst
substitutions (c : tl@(d : _)) rs = case M.lookup [c,d] rs of
                    Nothing -> c : substitutions tl rs
                    Just btwn -> c : btwn : substitutions tl rs

repeatedSubstitution :: Int -> Polymer -> Rules -> Polymer
repeatedSubstitution i p rs = if i == 0 then p 
  else repeatedSubstitution (i - 1) (substitutions p rs) rs

type Counts = M.Map Char Int

counts :: Polymer -> Counts
counts = foldl' (\cnts c -> M.insertWith (+) c 1 cnts) M.empty

answer :: Input -> Int
answer (Input p rs) = let subst = repeatedSubstitution 10 p rs
                          cnts = counts subst
                          mx = foldl' max 0 cnts
                          mn = foldl' min mx cnts
                      in mx - mn

part1 :: IO (Either ParseError Int)
part1 = (fmap answer) <$> input

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
                                               Nothing -> acc
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

repeats :: Rules -> Int -> PairCounts -> PairCounts
repeats rs i pcs = if i == 0 then pcs else repeats rs (i - 1) (applySubstitutions pcs rs)

repeatedly :: Int -> Input -> PairCounts
repeatedly i (Input p rs) = repeats rs i (countPairs p)

elements :: PairCounts -> [Char]
elements pcs = S.toList $ S.fromList $ concat $ M.keys pcs

answer2 :: Input -> Integer
answer2 inp@(Input p _) = let pcs = repeatedly 40 inp
                              f = head p
                              l = last p
                              elCounts = map (elementCount f l pcs) (elements pcs)
                          in maximum elCounts - minimum elCounts

part2 :: IO (Either ParseError Integer)
part2 = (fmap answer2) <$> input


