import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (nub)
import qualified Data.Map.Strict as Map

type Node = Int

type Graph = IM.IntMap IS.IntSet

countPathsDAG :: Graph -> Node -> Node -> Int
countPathsDAG g start target = evalState (go start) IM.empty
  where
    go :: Node -> State (IM.IntMap Int) Int
    go v
      | v == target = pure 1
      | otherwise = do
        memo <- get
        case IM.lookup v memo of
          Just ans -> pure ans
          Nothing -> do
            let nbs = IS.toList (IM.findWithDefault IS.empty v g)
            ans <- sum <$> mapM go nbs
            modify' (IM.insert v ans)
            pure ans

topologicalSort :: Graph -> [Node]
topologicalSort g = go initialZeroIndegree []
  where
    indegreeMap :: IM.IntMap Int
    indegreeMap =
      IM.foldlWithKey'
        (\acc v nbs ->
           foldl (\a nb -> IM.insertWith (+) nb 1 a) acc (IS.toList nbs))
        IM.empty
        g
    initialZeroIndegree :: [Node]
    initialZeroIndegree =
      [v | v <- IM.keys g, IM.findWithDefault 0 v indegreeMap == 0]
    go :: [Node] -> [Node] -> [Node]
    go [] sorted = reverse sorted
    go (v:vs) sorted =
      let nbs = IS.toList (IM.findWithDefault IS.empty v g)
          (indegreeMap', newZeros) =
            foldl
              (\(im, zeros) nb ->
                 let indeg = IM.findWithDefault 0 nb im - 1
                  in if indeg == 0
                       then (IM.delete nb im, nb : zeros)
                       else (IM.insert nb indeg im, zeros))
              (indegreeMap, [])
              nbs
       in go (vs ++ newZeros) (v : sorted)

parseInput :: String -> (Map.Map String Int, Graph)
parseInput s = (nameToInd, all_nodes)
  where
    s' = filter (/= ':') s
    nodes = nub $ words s'
    nameToInd :: Map.Map String Int
    nameToInd = Map.fromList $ zip nodes [0 ..]
    raw_nodes = map parseLine (lines s')
    zero_nodes = IM.fromList [(i, IS.empty) | i <- [0 .. (length nodes - 1)]]
    all_nodes :: Graph
    all_nodes = IM.union (IM.fromList raw_nodes) zero_nodes
    parseLine :: String -> (Int, IS.IntSet)
    parseLine line = (startInd, IS.fromList targetsInd)
      where
        wrds = words line
        start = head wrds
        targets = tail wrds
        startInd = Map.findWithDefault 0 start nameToInd
        targetsInd = map (\x -> Map.findWithDefault 0 x nameToInd) targets

part1 (nameToInd, g) = countPathsDAG g start end
  where
    start = Map.findWithDefault 0 "you" nameToInd
    end = Map.findWithDefault 0 "out" nameToInd

part2 (nameToInd, g) = product [startToFftPaths, fftToDacPaths, dacToEndPaths]
  where
    start = Map.findWithDefault 0 "svr" nameToInd
    fft = Map.findWithDefault 0 "fft" nameToInd
    dac = Map.findWithDefault 0 "dac" nameToInd
    end = Map.findWithDefault 0 "out" nameToInd
    startToFftPaths = countPathsDAG g start fft
    fftToDacPaths = countPathsDAG g fft dac
    dacToEndPaths = countPathsDAG g dac end

main :: IO ()
main = do
  test_input <- readFile "data/day11.txt"
  let testInput = parseInput test_input
  print $ part1 testInput
  print $ part2 testInput
