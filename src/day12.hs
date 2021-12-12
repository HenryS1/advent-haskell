module Day12 where

import Data.Foldable
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Control.Monad

data Cave = Big String | Small String deriving (Eq, Ord, Show)

parseCave :: GenParser Char st Cave
parseCave = (Big <$> many1 upper) <|> (Small <$> many1 lower)

parseEdge :: GenParser Char st (Cave, Cave)
parseEdge = do
  one <- parseCave
  _ <- char '-'
  other <- parseCave
  return (one, other)

type Graph = M.Map Cave [Cave]

withReverseEdges :: [(Cave, Cave)] -> [(Cave, Cave)]
withReverseEdges l = l ++ map (\(one, other) -> (other, one)) l

graphFromEdges :: [(Cave, Cave)] -> Graph
graphFromEdges = foldl' (\gr (one, other) -> M.insertWith (++) one [other] gr) M.empty

parseGraph :: GenParser Char st Graph
parseGraph = (graphFromEdges . withReverseEdges) <$> (many1 (parseEdge <* endOfLine))

input :: IO (Either ParseError Graph)
input = do
  fileContent <- TIO.readFile "/Users/henrysteere/wip/haskell/advent2021/src/day12.input"
  return $ parse parseGraph "day12.input" (T.unpack fileContent)

data SearchState = SearchState { current :: Cave, seen :: S.Set Cave, path :: [Cave], pathCount :: Int, singleSeen :: Maybe Cave }

countPaths :: Graph -> Cave -> ST.State SearchState ()
countPaths gr end = do
  s@(SearchState curr sn _ cnt _ ) <- ST.get
  if curr == end
    then ST.put (s { pathCount = cnt + 1})
    else case M.lookup curr gr of
           Nothing -> return ()
           Just cs -> foldM_ (\_ c -> 
                      if S.member c sn then pure ()
                      else do
                        curState <- ST.get
                        let updatedState = case c of
                             (Small _) -> (curState { 
                                              current = c, 
                                              seen = S.insert c (seen curState),
                                              path = c : (path curState)
                                              })
                             (Big _) -> (curState { current = c, path = c : (path curState) })
                        ST.put updatedState
                        countPaths gr end
                        newCnt <- pathCount <$> ST.get
                        ST.put (curState { current = curr, seen = sn, pathCount = newCnt })) () cs

answer :: Graph -> Int
answer gr = pathCount 
  $ ST.execState (countPaths gr (Small "end")) (SearchState (Small "start") (S.singleton (Small "start")) [] 0 Nothing)

start :: Cave 
start = Small "start"

countPathsSingleRepeat :: Graph -> Cave -> ST.State SearchState ()
countPathsSingleRepeat gr end = do
  s@(SearchState curr sn _ cnt singleS) <- ST.get
  if curr == end
    then ST.put (s { pathCount = cnt + 1 })
    else case M.lookup curr gr of
           Nothing -> return ()
           Just cs -> foldM_ (\_ c -> 
                      if c == start then pure ()
                      else if S.member c sn then
                        do 
                          state <- ST.get
                          case singleSeen state of
                            Just _ -> return ()
                            Nothing -> do
                              ST.put (state { 
                                         current = c,
                                         singleSeen = Just c,
                                         path = c : (path state)
                                         })
                              countPathsSingleRepeat gr end
                              nextNewSt <- ST.get
                              ST.put (state { pathCount = pathCount nextNewSt })
                      else do
                        curState <- ST.get
                        case c of 
                          Small _ -> do
                            ST.put (curState { current = c, seen = S.insert c (seen curState), path = c : (path curState) })
                            countPathsSingleRepeat gr end
                            firstNewSt <- ST.get
                            ST.put (curState { pathCount = pathCount firstNewSt })
                          Big _ -> do 
                            ST.put (curState { current = c, path = c : path curState })
                            countPathsSingleRepeat gr end
                            newSt <- ST.get
                            ST.put (curState { pathCount = pathCount newSt, singleSeen = singleS })) () cs

answerWithSingleSeen :: Graph -> Int
answerWithSingleSeen gr = pathCount $
  ST.execState (countPathsSingleRepeat gr (Small "end")) (SearchState (Small "start") (S.singleton (Small "start")) [Small "start"] 0 Nothing)

part1 :: IO (Either ParseError Int)
part1 = (fmap answer) <$> input

part2 :: IO (Either ParseError Int)
part2 = (fmap answerWithSingleSeen) <$> input
