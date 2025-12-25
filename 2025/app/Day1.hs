{-# LANGUAGE ScopedTypeVariables #-}

import Text.Read (readMaybe)

data Direction = R | L deriving (Show, Eq)

data Move = Move Direction Int deriving (Show, Eq)

-- Safer, total parsing of the direction
parseDirection :: Char -> Maybe Direction
parseDirection 'R' = Just R
parseDirection 'L' = Just L
parseDirection _   = Nothing

-- Safer parsing of a move using readMaybe instead of read
parseMove :: String -> Maybe Move
parseMove []       = Nothing
parseMove (d:rest) = do
  dir <- parseDirection d
  n   <- readMaybe rest
  pure (Move dir n)

parseInput :: String -> Maybe [Move]
parseInput = traverse parseMove . lines

modulus :: Int
modulus = 100

zeroCrosses :: Int -> Move -> (Int, Int)
zeroCrosses start (Move dir val) = (mod stop modulus, crossings)
  where
    stop =
      case dir of
        R -> start + val
        L -> start - val
    stop_mod = mod stop modulus
    crossings =
      case dir of
        R -> div (stop - 1) modulus - div start modulus
        L -> div (start - 1) modulus - div stop modulus

accumZeroCrosses start = scanl step (start, 0)
  where
    step (pos, _) = zeroCrosses pos

part1 :: Int -> [Move] -> Int
part1 start = length . filter ((== 0) . fst) . accumZeroCrosses start

part2 :: Int -> [Move] -> Int
part2 start moves = crossings + zeroVisits
  where
    stats = accumZeroCrosses 50 moves
    crossings = sum $ map snd stats
    zeroVisits = part1 start moves


main :: IO ()
main = do
  input <- readFile "data/day1.txt"
  case parseInput input of
    Nothing -> putStrLn "Failed to parse input."
    Just moves -> do
      print $ part1 50 moves
      print $ part2 50 moves
