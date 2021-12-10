module Day10 where

import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.List as L

data Bracket = Open Int | Close Int deriving (Ord, Eq, Show)

parseBracket :: GenParser Char st Bracket
parseBracket = (pure (Open 1) <* char '(') 
  <|> (pure (Open 2) <* char '[') 
  <|> (pure (Open 3) <* char '{')
  <|> (pure (Open 4) <* char '<')
  <|> (pure (Close 1) <* char ')')
  <|> (pure (Close 2) <* char ']')
  <|> (pure (Close 3) <* char '}')
  <|> (pure (Close 4) <* char '>')

parseLines :: GenParser Char st [[Bracket]]
parseLines = many1 ((many1 parseBracket) <* endOfLine)

input :: IO (Either ParseError [[Bracket]])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day10.input"
  return $ parse parseLines "day10.input" (T.unpack fileContent)

findInvalidBracket :: [Bracket] -> Maybe Bracket
findInvalidBracket s = 
  let findInvalidChar' :: (Maybe Bracket) -> [Bracket] -> Either Bracket [Bracket]
      findInvalidChar' _ [] = Right []
      findInvalidChar' Nothing (c : cs) = findInvalidChar' (Just c) cs
      findInvalidChar' curr@(Just open) (c : cs) = 
        case (open, c) of
          (_, o@(Open _)) -> case (findInvalidChar' (Just o) cs) of
            l@(Left _) -> l
            Right rest -> findInvalidChar' curr rest
          (Open i, Close j) -> if i == j
            then Right cs
            else Left c
          (_, Close _) -> Left c
  in case findInvalidChar' Nothing s of
      Left b -> Just b
      Right _ -> Nothing

scoreMap :: M.Map Bracket Int
scoreMap = M.fromList [(Close 1, 3), (Close 2, 57), (Close 3, 1197), (Close 4, 25137)]

illegalCharactersScore :: [[Bracket]] -> Int
illegalCharactersScore = foldl' (\score bs -> 
                                   case findInvalidBracket bs >>= ((flip M.lookup) scoreMap) of
                                     Nothing -> score 
                                     Just s -> score + s) 0

part1 :: IO (Either ParseError Int)
part1 = (fmap illegalCharactersScore) <$> input

findCompletion :: [Bracket] -> Maybe [Bracket]
findCompletion brackets = 
  let completion' :: [Bracket] -> [Bracket] -> Maybe [Bracket]
      completion' [] [] = Nothing
      completion' bs [] = Just bs
      completion' [] (Close _ : _) = Nothing
      completion' (Close _ : _) _ = Nothing
      completion' os (o@(Open _) : bs) = completion' (o : os) bs
      completion' (Open i : os) (Close j : bs) = 
        if i == j 
        then completion' os bs
        else Nothing
  in completion' [] brackets

completionMap :: M.Map Bracket Int
completionMap = M.fromList [(Open 1, 1), (Open 2, 2), (Open 3, 3), (Open 4, 4)]

scoreCompletion :: [Bracket] -> Maybe Int
scoreCompletion bs = foldl' (\total b -> case M.lookup b completionMap of
                                Just i -> ((i+) . (*5)) <$> total
                                Nothing -> Nothing) (Just 0) bs

scoreCompletions :: [[Bracket]] -> [Int]
scoreCompletions bs = catMaybes $ (map scoreCompletion (catMaybes $ map findCompletion bs))

middleScore :: [Int] -> Maybe Int
middleScore scores = let len = length scores 
                     in fst <$> (L.uncons $ drop (len `div` 2) (L.sort scores))

part2 :: IO (Either ParseError (Maybe Int))
part2 = (fmap (middleScore . scoreCompletions)) <$> input
