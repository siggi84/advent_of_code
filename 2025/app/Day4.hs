import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)

parseInput :: String -> Set Coord
parseInput s =
  Set.fromList
    [ (lineIndex, charIndex)
    | (lineIndex, line) <- zip [0 ..] (lines s)
    , (charIndex, '@') <- zip [0 ..] line
    ]

-- Generic fixed-point combinator
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
  | x' == x = x
  | otherwise = fixpoint f x'
  where
    x' = f x

numNeighbors :: Set Coord -> Coord -> Int
numNeighbors rs (x, y) = length $ filter (`Set.member` rs) cands
  where
    cands =
      [ (x + dx, y + dy)
      | dx <- [-1 .. 1]
      , dy <- [-1 .. 1]
      , not (dx == 0 && dy == 0)
      ]

part1 :: Set Coord -> Int
part1 rs = length $ Set.filter ((< 4) . numNeighbors rs) rs

part2 :: Set Coord -> Int
part2 rs = length $ Set.difference rs (fixpoint step rs)
  where
    step s = Set.filter ((<= 4) . numNeighbors s) s

main :: IO ()
main = do
  input <- readFile "data/day4.txt"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input
