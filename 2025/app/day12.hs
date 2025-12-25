parseInput :: String -> [String]
parseInput = lines

part1 s = s

main :: IO ()
main = do
  test_input <- readFile "data/day12_test.txt"
  let testInput = parseInput test_input
  print $ part1 testInput
