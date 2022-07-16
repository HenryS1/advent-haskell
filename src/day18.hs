module Day18 where

import Data.Maybe
import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit

data Tree = Node Int Tree Tree
  | Leaf Int  

treeHeight :: Tree -> Int
treeHeight (Node h _ _) = h
treeHeight (Leaf _) = 0

instance Show Tree where
  show (Leaf v) = show v
  show (Node _ left right) = '[' : show left ++ ',' : show right ++ "]"

parseLeaf :: GenParser Char st Tree
parseLeaf = Leaf <$> positiveInt

parseNode :: GenParser Char st Tree
parseNode = do
  _ <- char '['
  left <- parseTree
  _ <- char ','
  right <- parseTree
  _ <- char ']'
  return (Node (1 + (treeHeight left `max` treeHeight right)) left right)

parseTree :: GenParser Char st Tree
parseTree = parseLeaf <|> parseNode

parseTrees :: GenParser Char st [Tree]
parseTrees = many1 (parseTree <* endOfLine)

input :: IO (Either ParseError [Tree])
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day18.input"
  return $ parse parseTrees "day18.input" (T.unpack fileContent)

addTrees :: Tree -> Tree -> Tree
addTrees one two = Node (1 + (treeHeight one `max` treeHeight two)) one two

data SnailError = ExplodeNonPair Tree | NothingToExplode Tree | NoTrees deriving Show

addToLeft :: Int -> Tree -> Tree
addToLeft i (Leaf v) = Leaf (i + v)
addToLeft i (Node h l r) = Node h (addToLeft i l) r

addToRight :: Int -> Tree -> Tree
addToRight i (Leaf v) = Leaf (i + v)
addToRight i (Node h l r) = Node h l (addToRight i r)

explode :: Tree -> Either SnailError (Tree, Bool)
explode tr = (\(t, mb) -> (t, isJust mb)) <$> explode' 4 tr
  where explode' _ l@(Leaf _) = Right (l, Nothing)
        explode' 0 (Node _ (Leaf v1) (Leaf v2)) = Right ((Leaf 0), Just (v1, v2))
        explode' 0 n@(Node _ _ _) = Left (ExplodeNonPair n)
        explode' i n@(Node h l r) = if h <= i 
          then Right (n, Nothing)
          else case explode' (i - 1) l of
                 err@(Left _) -> err
                 Right (newL, Just vs@(v1, v2)) ->
                   if v2 > 0 then
                     let newR = addToLeft v2 r
                     in Right (Node (1 + (treeHeight newL `max` treeHeight newR)) newL newR, Just (v1, 0))
                   else Right (Node (1 + (treeHeight newL `max` treeHeight r)) newL r, Just vs)
                 Right (_, Nothing) -> 
                   case explode' (i - 1) r of
                     err@(Left _) -> err
                     Right (_, Nothing) -> Left (NothingToExplode r)
                     Right (newR, Just vs@(v1, v2)) -> 
                       if v1 > 0 then 
                         let newL = addToRight v1 l
                         in Right (Node (1 + (treeHeight newL `max` treeHeight newR)) newL newR, Just (0, v2))
                       else Right (Node (1 + (treeHeight l `max` treeHeight newR)) l newR, Just vs)


split :: Tree -> (Tree, Bool)
split l@(Leaf v) = if v >= 10 
          then let (d, r) = v `divMod` 2
          in ((Node 1 (Leaf d) (Leaf (d + r))), True)
          else (l, False)
split n@(Node _ l r) = case split l of
  (_, False) -> case split r of
    (_, False) -> (n, False)
    (newR, True) -> (Node (1 + (treeHeight l `max` treeHeight newR)) l newR, True)
  (newL, True) -> (Node (1 + (treeHeight newL `max` treeHeight r)) newL r, True)

reduce :: Tree -> Either SnailError Tree
reduce tr = case explode tr of
  Left err -> Left err
  Right (_, False) -> case split tr of
    (_, False) -> Right tr
    (newTr, True) -> reduce newTr
  Right (newTr, True) -> reduce newTr

snailSum :: Tree -> Tree -> Either SnailError Tree
snailSum one other = reduce (addTrees one other)

snailTotal :: [Tree] -> Either SnailError Tree
snailTotal [] = Left NoTrees
snailTotal (first : rest) = foldl' (\acc t -> acc >>= (reduce . (\other -> addTrees other t))) (Right first) rest

magnitude :: Tree -> Int
magnitude (Leaf v) = v
magnitude (Node _ l r) = 3 * magnitude l + 2 * magnitude r

chooseTwo :: [a] -> [(a, a)]
chooseTwo as = chooseTwo' Nothing as
  where chooseTwo' _ [] = []
        chooseTwo' Nothing [_] = []
        chooseTwo' Nothing (a : rest) = chooseTwo' (Just a) rest ++ chooseTwo' Nothing rest
        chooseTwo' j@(Just one) (other : rest) =
          (one, other) : (chooseTwo' j rest)

largestPairMagnitude :: (Tree, Tree) -> Either SnailError Int
largestPairMagnitude (one, other) = 
  case (magnitude <$> reduce (addTrees one other), magnitude <$> reduce (addTrees other one)) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right m1, Right m2) -> Right (m1 `max` m2)

largestMagnitude :: [Tree] -> Either SnailError Int
largestMagnitude ts = 
  let ms = map largestPairMagnitude $ chooseTwo ts
  in foldl' (\acc m -> case (acc, m) of
                (l@(Left _), _) -> l
                (_, l@(Left _)) -> l
                (bst@(Right old), Right new) -> if new > old then Right new else bst) (Right 0) ms

data CombinedError = ParseFailed ParseError | ArithmeticError SnailError deriving Show

part1 :: IO (Either CombinedError Int)
part1 = do
  parsedTrees <- input
  return $ case parsedTrees of
    Left err -> Left (ParseFailed err)
    Right trees -> case magnitude <$> snailTotal trees of
      Left err -> Left (ArithmeticError err)
      Right m -> Right m

part2 :: IO (Either CombinedError Int)
part2 = do
  parsedTrees <- input
  return $ case parsedTrees of
    Left err -> Left (ParseFailed err)
    Right trees -> case largestMagnitude trees of
      Left err -> Left (ArithmeticError err)
      Right m -> Right m
