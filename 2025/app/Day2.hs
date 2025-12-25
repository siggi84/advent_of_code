import Text.Parsec
import Text.Parsec.String (Parser)

--- Domain model
data Range = Range Int Int deriving (Show, Eq)

--- Parser
int :: Parser Int
int = read <$> many1 digit

range :: Parser Range
range = Range <$> int <* char '-' <*> int

commaSep :: Parser ()
commaSep = char ',' *> spaces

ranges :: Parser [Range]
ranges = range `sepBy1` commaSep

parseRanges :: String -> Either ParseError [Range]
parseRanges =
  parse (spaces *> ranges <* spaces <* eof) "ranges"

rangeMap :: Range -> [Int]
rangeMap (Range start stop) = [start .. stop]

isRepeat :: Int -> String -> Bool
isRepeat m s = m > 0 && mod n m == 0 && take n (cycle prefix) == s
  where
    n = length s
    prefix = take m s

countValid :: (Int -> Bool) -> [Range] -> Int
countValid p = sum . filter p . concatMap rangeMap

part1 :: [Range] -> Int
part1 = countValid isInvalid
  where
    isInvalid :: Int -> Bool
    isInvalid num =
      let s = show num
          n = length s
      in even n && isRepeat (div n 2) s 

part2 :: [Range] -> Int
part2 = countValid isInvalid
  where
    isInvalid :: Int -> Bool
    isInvalid num =
      let s = show num
          n = length s
          n2 = div n 2
      in any (`isRepeat` s) [1 .. n2]

main :: IO ()
main = do
  input <- readFile "data/day2.txt"
  case parseRanges input of
    Left err -> putStrLn $ "Failed to parse input: " <> show err
    Right ranges -> do
      print $ part1 ranges
      print $ part2 ranges
