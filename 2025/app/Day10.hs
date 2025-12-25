{-# LANGUAGE DerivingStrategies #-}

import Data.Bits (Bits(..), countTrailingZeros, popCount)
import qualified Data.Map.Strict as M

import qualified Data.IntSet as IS
import Data.List (foldl', subsequences)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))
import qualified Data.Set as S
import Data.Traversable (sequenceA)
import qualified Data.Vector as V
import Debug.Trace

type Mask = Int -- or Word64

data Button =
  Button
    { bMask :: !Mask -- which lights this button toggles
    , bIdxs :: !(V.Vector Int) -- which lights this button toggles
    }
  deriving (Show, Eq)

data Machine =
  Machine
    { mLightsN :: !Int
    , mTarget :: !Mask -- desired final pattern; initial is always 0
    , mButtons :: !(V.Vector Button)
    , mJolts :: !(V.Vector Int)
    }
  deriving (Show, Eq)

parseInput :: String -> [Machine]
parseInput s = map parseMachine ls
  where
    ls = lines s

parseMachine :: String -> Machine
parseMachine line = Machine mLightsN lMask buttonsVec jolts
  where
    ws = words line
    lightsTrim = init $ tail (head ws)
    mLightsN = length lightsTrim
    buttonStrings :: [[Int]]
    buttonStrings = map (map read . splitOn "," . init . tail) $ init $ tail ws
    toMask :: [Int] -> Mask
    toMask indices = sum [1 `shiftL` i | i <- indices]
    buttonsVec =
      V.fromList [Button (toMask bs) (V.fromList bs) | bs <- buttonStrings]
    lMask = bitmaskBinary (reverse lightsTrim)
    bitmaskBinary :: String -> Mask
    bitmaskBinary = foldl step 0
      where
        step acc c =
          case c of
            '.' -> acc * 2
            '#' -> acc * 2 + 1
            _ -> error ("bitmaskBinary: invalid char " ++ show c)
    jolts :: V.Vector Int
    jolts = V.fromList $ map read $ splitOn "," $ init $ tail $ last ws

solveLightsBFS :: Machine -> Maybe Int
solveLightsBFS m = bfs (Seq.singleton (0, 0)) (IS.singleton 0)
  where
    target :: Mask
    target = mTarget m
    btns :: V.Vector Mask
    btns = V.map bMask (mButtons m)
    bfs :: Seq (Mask, Int) -> IS.IntSet -> Maybe Int
    bfs q visited =
      case Seq.viewl q of
        Seq.EmptyL -> Nothing
        (state, d) Seq.:< q'
          | state == target -> Just d
          | otherwise ->
            let (qNext, visNext) = V.foldl' (step state d) (q', visited) btns
             in bfs qNext visNext
    step ::
         Mask
      -> Int
      -> (Seq (Mask, Int), IS.IntSet)
      -> Mask
      -> (Seq (Mask, Int), IS.IntSet)
    step state d (q, vis) bm =
      let s' = state `xor` bm
       in if IS.member s' vis
            then (q, vis)
            else (q |> (s', d + 1), IS.insert s' vis)

part1 :: [Machine] -> Maybe Int
part1 machines = sum <$> traverse solveLightsBFS machines

maskTo01Vec :: Int -> Int -> V.Vector Int
maskTo01Vec n m =
  V.generate
    n
    (\i ->
       if testBit m i
         then 1
         else 0)

solveJoltsP m = go jolts
  where
    jolts = mJolts m
    n = V.length jolts
    buttons = mButtons m
    nb = V.length buttons
    btnParity :: V.Vector Mask
    btnParity = V.map bMask buttons
    btnVecs :: V.Vector (V.Vector Int)
    btnVecs = V.map (maskTo01Vec n) btnParity
    zero = V.replicate n 0
    parityMaskOf :: V.Vector Int -> Mask
    parityMaskOf =
      V.ifoldl'
        (\acc i x ->
           if odd x
             then setBit acc i
             else acc)
        0
    halfVec :: V.Vector Int -> V.Vector Int
    halfVec = V.map (`div` 2)
    -- Precompute all phase-1 subsets p in {0,1}^m
    -- For each subset mask s: store (parityMask, contribVec, popcount)
    subsets :: V.Vector (Mask, V.Vector Int, Int)
    subsets = V.generate (1 `shiftL` nb) build
      where
        build 0 = (0, zero, 0)
        build s =
          let lsb = countTrailingZeros s
              s' = clearBit s lsb
              (pm, pv, pc) = subsets V.! s'
              pm' = pm `xor` (btnParity V.! lsb)
              pv' = V.zipWith (+) pv (btnVecs V.! lsb)
              pc' = pc + 1
           in (pm', pv', pc')
    go :: V.Vector Int -> Maybe Int
    go b
      | V.all (== 0) b = Just 0
      | otherwise =
        let targetParity = parityMaskOf b
            candidates =
              [ cost + 2 * ans2
              | (pm, pv, cost) <- V.toList subsets
              , pm == targetParity
              , let remv = V.zipWith (-) b pv
              , V.all (>= 0) remv
              , V.all even remv
              , let b2 = halfVec remv
              , Just ans2 <- [go b2]
              ]
         in case candidates of
              [] -> Nothing
              xs ->
                let ans = minimum xs
                 in Just ans

part2 machines = sum <$> traverse solveJoltsP machines

main :: IO ()
main = do
  test_input <- readFile "data/day10.txt"
  let testInput = parseInput test_input
  print $ part1 testInput
  print $ part2 testInput
