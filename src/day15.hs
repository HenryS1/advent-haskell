module Day15 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Array as A
import qualified Data.Char as C
import qualified Data.Set as S

data LeftistHeap a = Empty | NonEmpty { 
  rankNum :: Int,
  x :: a,
  left :: LeftistHeap a,
  right ::  LeftistHeap a
} deriving Show

insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert el heap = mergeHeaps (NonEmpty 1 el Empty Empty) heap

rank :: LeftistHeap a -> Int
rank (NonEmpty r _ _ _) = r
rank Empty = 0

makeTree :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeTree el one other = if rank one >= rank other
  then NonEmpty (rank other + 1) el one other
  else NonEmpty (rank one + 1) el other one

mergeHeaps :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
mergeHeaps one Empty = one
mergeHeaps Empty other = other
mergeHeaps one@(NonEmpty _ el1 l1 r1) other@(NonEmpty _ el2 l2 r2) =
  if el1 < el2
  then makeTree el1 l1 (mergeHeaps r1 other)
  else makeTree el2 l2 (mergeHeaps r2 one)

data HeapError = DeleteCalledOnEmptyHeap

findMin :: Ord a => LeftistHeap a -> Maybe a
findMin Empty = Nothing
findMin (NonEmpty _ e _ _) = Just e

deleteMin :: Ord a => LeftistHeap a -> Maybe (a, LeftistHeap a)
deleteMin Empty = Nothing
deleteMin (NonEmpty _ e l r) = Just (e, mergeHeaps l r)

heapToList :: LeftistHeap a -> [a]
heapToList Empty = []
heapToList (NonEmpty _ a l r) = a : heapToList r ++ heapToList l

parseDigit :: GenParser Char st Int
parseDigit = C.digitToInt <$> digit

parseRow :: GenParser Char st [Int]
parseRow = many1 parseDigit 

data Grid = Grid { rows :: Int, columns :: Int, entries :: A.Array Int Int } deriving Show

parseGrid :: GenParser Char st Grid
parseGrid = do
  first <- parseRow <* endOfLine
  rest <- many1 (parseRow <* endOfLine)
  let allRows = first : rest
  let rs = length allRows
  let cs = length first
  return (Grid rs cs (A.listArray (0, rs * cs - 1) (concat allRows)))

incd :: Int -> Int
incd v = ((v + 1) `mod` 10) `max` 1

incMod :: Int -> Int -> Int
incMod i v = if i == 0 then v else incMod (i - 1) (incd v)


parseRowFiveTimes :: GenParser Char st [Int]
parseRowFiveTimes = do
  r <- many1 parseDigit
  let reps = r : map (incMod 1) r : map (incMod 2) r : map (incMod 3) r : [map (incMod 4) r]
  return $ concat reps

parseGrid25Times :: GenParser Char st Grid
parseGrid25Times = do
  first <- parseRowFiveTimes <* endOfLine 
  rest <- many1 (parseRowFiveTimes <* endOfLine)
  let allR = first : rest
  let rs = length allR * 5
  let cs = length first
  let cncd = concat allR
  let reps = cncd : map (incMod 1) cncd : map (incMod 2) cncd : map (incMod 3) cncd : [map (incMod 4) cncd]
  return $ Grid rs cs (A.listArray (0, rs * cs - 1) (concat reps))

input :: IO (Either ParseError Grid)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day15.input"
  return $ parse parseGrid "day15.input" (T.unpack fileContent)

inputConcat :: IO (Either ParseError Grid)
inputConcat = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day15.input"
  return $ parse parseGrid25Times "day15.input" (T.unpack fileContent)

closeBy :: Grid -> Int -> Int -> Bool
closeBy (Grid _ cs _) i j = let (r1, c1) = i `divMod` cs
                                (r2, c2) = j `divMod` cs
                            in abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

neighbours :: Grid -> Int -> [Int]
neighbours gr@(Grid rs cs _) i = 
  filter (\n -> closeBy gr i n && n >= 0 && n <= rs * cs - 1) [i - 1, i + 1, i - cs, i + cs]

data SearchState = SearchState { risk :: Int, current :: Int, path :: [Int] } deriving (Eq, Show)

instance Ord SearchState where
  (<=) one other = risk one <= risk other

type Seen = S.Set Int

findLowestRisk :: LeftistHeap SearchState -> Seen -> Grid -> Maybe SearchState
findLowestRisk hp sn gr@(Grid rs cs es) = case deleteMin hp of
  Nothing -> Nothing
  Just (st, newHp) -> 
    if current st == rs * cs - 1
    then Just st
    else let ns = filter (not . ((flip S.member) sn)) $ neighbours gr (current st)
             heapWithNbrs = foldl' (\h n -> insert (SearchState { risk = risk st + es A.! n, current = n, path = path st }) h) newHp ns
             newSeen = foldl' (\acc e -> S.insert e acc) sn ns
         in findLowestRisk heapWithNbrs newSeen gr

part1 :: IO (Either ParseError (Maybe SearchState))
part1 = do
  parsedGrid <- input
  case parsedGrid of 
    Left err -> return $ Left err
    Right grid ->
      let initState = (SearchState { risk = 0, current = 0, path = [0] })
      in return $ Right $ findLowestRisk (insert initState Empty) (S.singleton 0) grid

incrementGrid :: Grid -> Grid
incrementGrid (Grid rs cs es) = 
  let indices = A.listArray (0, rs * cs - 1) [0..(rs * cs - 1)]
  in Grid rs cs (fmap (\i -> ((es A.! i + 1) `mod` 10) `max` 1) indices)

type Entries = A.Array Int Int

part2 :: IO (Either ParseError (Maybe SearchState))
part2 = do
  parsedGrid <- inputConcat
  case parsedGrid of 
    Left err -> return $ Left err
    Right grid ->
      let initState = (SearchState { risk = 0, current = 0, path = [0] })
      in return $ Right $ findLowestRisk (insert initState Empty) (S.singleton 0) grid
