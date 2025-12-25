import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String (Parser)

type Bank = [Int]

bank :: Parser Bank
bank = map digitToInt <$> many1 digit

banks :: Parser [Bank]
banks = bank `sepEndBy1` (endOfLine <* spaces)

parseBanks :: String -> Either ParseError [Bank]
parseBanks =
  parse (spaces *> banks <* eof) "banks"

maxJolt num bks | length bks < num = Nothing
                | otherwise          = Just $ go num bks
  where
  go :: Int -> Bank -> Int
  go 0 _ = 0
  go n b = first * 10 ^ (n - 1) + rest
    where
      first = maximum $ drop (n - 1) $ reverse b
      rest = go (n-1) $ tail $ dropWhile (< first) b

part1 bs = sum <$> traverse (maxJolt 2) bs
part2 bs = sum <$> traverse (maxJolt 12) bs

main :: IO ()
main = do
  input <- readFile "data/day3.txt"
  case parseBanks input of
    Left err -> putStrLn $ "Failed to parse input: " <> show err
    Right banks -> do
      print $ part1 banks
      print $ part2 banks
