import Data.List (foldl', transpose)
import Text.Parsec
import Text.Parsec.String (Parser)

data Op
  = Add
  | Mul
  deriving (Eq, Show)

data Grid =
  Grid
    { numbers :: [[Int]]
    , operators :: [Op]
    }
  deriving (Show, Eq)

parseGrid :: String -> Either ParseError Grid
parseGrid = parse grid "<input>"

grid :: Parser Grid
grid = do
  ns <- many1 numLine
  ops <- opLine
  hspace *> eof
  pure (Grid ns ops)

-- ---- line parsers ----
numLine :: Parser [Int]
numLine = do
  hspace
  lookAhead digit
  xs <- int `sepEndBy1` hspace1
  lineEnd
  pure xs

opLine :: Parser [Op]
opLine = do
  hspace
  ops <- opTok `sepEndBy1` hspace1
  lineEnd
  pure ops

int :: Parser Int
int = read <$> many1 digit

opTok :: Parser Op
opTok = (char '+' *> pure Add) <|> (char '*' *> pure Mul)

hspace :: Parser ()
hspace = skipMany (oneOf " \t")

hspace1 :: Parser ()
hspace1 = skipMany1 (oneOf " \t")

lineEnd :: Parser ()
lineEnd = hspace *> (endOfLine *> pure () <|> eof)

opToFunc :: Op -> (Int -> Int -> Int)
opToFunc Add = (+)
opToFunc Mul = (*)

opToIdentity :: Op -> Int
opToIdentity Add = 0
opToIdentity Mul = 1

applyOp :: Op -> [Int] -> Int
applyOp o = foldl' (opToFunc o) (opToIdentity o)

-- part1 :: Grid -> Int
part1 (Grid ns ops) = sum $ map (uncurry applyOp) $ zip ops $ transpose ns
  where
    nsT = transpose ns

part2 s ops = sum $ map (uncurry applyOp) $ zip ops (splitOnEmptyInts numsT)
  where
    numsT = transpose $ init $ lines s
    splitOnEmptyInts :: [String] -> [[Int]]
    splitOnEmptyInts = map (map read) . splitOnEmpty
    allSpaces = all (== ' ')
    splitOnEmpty :: [String] -> [[String]]
    splitOnEmpty [] = []
    splitOnEmpty xs =
      let (first, rest) = break allSpaces xs
          remaining = dropWhile allSpaces rest
       in first : splitOnEmpty remaining

main :: IO ()
main = do
  input <- readFile "data/day6.txt"
  case parseGrid input of
    Left err -> putStrLn $ "Failed to parse input: " <> show err
    Right grid -> do
      print $ part1 grid
      print $ part2 input (operators grid)
