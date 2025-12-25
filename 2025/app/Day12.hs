import Data.List (sort)
import Data.List.Split (splitOn)

type Cell = (Int, Int)

type Variant = [Cell]

data Box =
  Box
    { bSize :: (Int, Int)
    , bPresents :: [Int]
    }
  deriving (Show, Eq)

-- parseInput :: String -> [String]
parseInput :: String -> ([Variant], [Box])
parseInput s = (presentsCells, boxesRet)
    -- A bit nasty parsing, but it does the job
  where
    sections = splitOn "\n\n" s
    presents = filter ('#' `elem`) sections
    presentsCells = map go presents
    go p =
      sort
        [ (l, c)
        | (c, line) <- zip [0 ..] (tail $ lines p)
        , (l, ch) <- zip [0 ..] line
        , ch == '#'
        ]
    boxes = filter (\x -> '#' `notElem` x) sections
    boxesLs = lines $ head boxes
    boxesSizes :: [(Int, Int)]
    boxesSizes =
      map
        ((\x -> (read $ head x, read $ head $ tail x)) .
         splitOn "x" . head . splitOn ":")
        boxesLs
    boxesDist :: [[Int]]
    boxesDist =
      map (map read . tail . splitOn " " . head . tail . splitOn ":") boxesLs
    boxesRet = zipWith Box boxesSizes boxesDist

-- This is ridiculous; but as the problem is NP-hard, I will not bother implementing a proper packing algorithm
part1 (presents, boxes) =
  sum $
  zipWith
    (\x y ->
       if x <= y
         then 1
         else 0) boxesPackable boxesSizes
  where
    presentsSizes = map length presents
    boxesSizes = map (uncurry (*) . bSize) boxes
    boxesPackable =
      [ sum $ zipWith (*) (bPresents b) presentsSizes
      | b <- boxes
      ]
    tmp = 0

main :: IO ()
main = do
  test_input <- readFile "data/day12.txt"
  let testInput = parseInput test_input
  print $ part1 testInput
