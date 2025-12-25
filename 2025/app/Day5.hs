import Data.List (sortOn)
import Text.Parsec
import Text.Parsec.String (Parser)

type Range = (Int, Int)

parseRangesAndValues :: String -> Either ParseError ([Range], [Int])
parseRangesAndValues = parse fileP "input"

fileP :: Parser ([Range], [Int])
fileP = do
  ranges <- sepEndBy1 rangeP endOfLine
  skipMany1 endOfLine
  values <- sepEndBy1 intP endOfLine
  eof
  pure (ranges, values)

rangeP :: Parser Range
rangeP = (,) <$> intP <* char '-' <*> intP

intP :: Parser Int
intP = read <$> many1 digit

rangeLength :: Range -> Int
rangeLength (lo, hi) = hi - lo + 1

valueInRange :: Range -> Int -> Bool
valueInRange (lo, hi) v = (v >= lo) && (v <= hi)

part1 :: [Range] -> [Int] -> Int
part1 ranges = length . filter (\v -> any (`valueInRange` v) ranges)

merge :: [Range] -> [Range]

merge = foldr step [] . sortOn fst
  where
    step :: Range -> [Range] -> [Range]
    step r [] = [r]
    step (l1, u1) acc@((l2, u2) : rs)
      | l2 > u1  = (l1, u1) : acc
      | otherwise = (l1, max u1 u2) : rs

part2 :: [Range] -> Int
part2 ranges = sum $ map rangeLength $ merge ranges

main :: IO ()
main = do
  input <- readFile "data/day5.txt"
  case parseRangesAndValues input of
    Left err -> putStrLn $ "Failed to parse input: " <> show err
    Right (ranges, values) -> do
      print $ part1 ranges values
      print $ part2 ranges
