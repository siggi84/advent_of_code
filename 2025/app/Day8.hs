import Data.List (find, sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Coord3D = (Int, Int, Int)

dist2 c1 c2 = sqDiff x1 x2 + sqDiff y1 y2 + sqDiff z1 z2
  where
    (x1, y1, z1) = c1
    (x2, y2, z2) = c2
    sqDiff a b = (a - b) * (a - b)

enumerate = zip [0 ..]

parseInput s = map (lToCoord3D . splitOn ",") $ lines s
  where
    lToCoord3D :: [String] -> Coord3D
    lToCoord3D [x, y, z] = (read x, read y, read z)
    listToCoord3D _ = error "Invalid coordinate format"

transposeMap :: (Ord v) => Map.Map k v -> Map.Map v [k]
transposeMap = Map.fromListWith (++) . map (\(k, v) -> (v, [k])) . Map.toList

helper coords = zip distancesIndexes (tail stream)
  where
    stream = scanl comb groups distances
    n = length coords
    distances =
      sortOn snd $
      [ ((i1, i2), dist2 c1 c2)
      | (i1, c1) <- enumerate coords
      , (i2, c2) <- enumerate coords
      , i1 < i2
      ]
    distancesIndexes = map fst distances
    groups = Map.fromList $ [(idx, idx) | idx <- [0 .. n - 1]]
    comb :: Map.Map Int Int -> ((Int, Int), Int) -> Map.Map Int Int
    comb g d = gUpdated
      where
        ((i1, i2), _) = d
        g1 = Map.findWithDefault i1 i1 g
        g2 = Map.findWithDefault i2 i2 g
        tMap = transposeMap g
        toUpdate =
          Map.fromList $ map (\x -> (x, g1)) $ Map.findWithDefault [] g2 tMap
        gUpdated = Map.union toUpdate g

part1 coords num = res
  where
    (_, tmp) = helper coords !! (num - 1)
    ds = Map.elems $ Map.map length $ transposeMap tmp
    res = product $ take 3 $ reverse $ sort ds

part2 coords = x1 * x2
  where
    stream = helper coords
    ((c1idx, c2idx), _) =
      head $ dropWhile (\(c, s) -> Map.size (transposeMap s) > 1) stream
    (x1, _, _) = coords !! c1idx
    (x2, _, _) = coords !! c2idx

main :: IO ()
main = do
  test_input <- readFile "data/day8_test.txt"
  let test_coords = parseInput test_input
  input <- readFile "data/day8.txt"
  let coords = parseInput input
  print $ part1 test_coords 10
  print $ part1 coords 1000
  print $ part2 test_coords
  print $ part2 coords
