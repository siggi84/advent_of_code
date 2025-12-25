import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)
type Size = (Int, Int)

parseInput :: String -> (Coord, Set Coord, Size)
parseInput s = (start, splitters, size)
  where
    n = length $ lines s
    m = length $ head $ lines s
    size = (n, m)
    find c =
      [ (lineIndex, charIndex)
      | (lineIndex, line) <- zip [0 ..] (lines s)
      , (charIndex, c') <- zip [0 ..] line
      , c' == c
      ]
    splitters = Set.fromList $ find '^'
    start = head $ find 'S'

-- part1 :: (Coord, Set Coord, (Int, Int)) -> Set Coord
part1 (start, splitters, (n, m)) = snd $ last tmp
  where
    tmp = take n $ iterate go (Set.fromList [start], 0)
    down (x, y) = (x + 1, y)
    split (x, y) = [(x, y - 1), (x, y + 1)]
    go (beams, nsplits) =
      (Set.union afterSplits nonSplits, nsplits + length splits)
      where
        beams' = Set.map down beams
        splits = Set.intersection beams' splitters
        nonSplits = Set.difference beams' splits
        afterSplits = Set.fromList $ concat $ Set.map split splits

part2 (start, splitters, (n, m)) =
  sum $ Map.elems $ (iterate go (Map.fromList [(start, 1)]) !! (n-1))
  where
    down' (x, y)
      | Set.member (x + 1, y) splitters = [(x + 1, y - 1), (x + 1, y + 1)]
      | otherwise = [(x + 1, y)]
    go :: Map.Map Coord Int -> Map.Map Coord Int
    go beams =
      Map.fromListWith (+) [(k', v) | (k, v) <- Map.toList beams, k' <- down' k]

main :: IO ()
main = do
  input <- readFile "data/day7.txt"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input
