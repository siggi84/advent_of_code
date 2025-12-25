{-# LANGUAGE BangPatterns #-}

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.List (nub, sort, group)
import qualified Data.Map as Map
import Data.Traversable (sequenceA)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word64)
import qualified Data.IntSet as IS

type Coord = (Int, Int)

type Area = (Coord, Coord)

parseInput :: String -> [Coord]
parseInput = lineToCoords
  where
    lineToCoords :: String -> [Coord]
    lineToCoords line =
      [ (read x, read y)
      | pair <- words line
      , let (x, ',':y) = break (== ',') pair
      ]

normalize :: Area -> Area
normalize ((x1, y1), (x2, y2)) =
  ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))


overlapArea :: Area -> Area -> Int
overlapArea a1 a2
  | w <= 0 || h <= 0 = 0
  | otherwise = w * h
  where
    ((x1min, y1min), (x1max, y1max)) = normalize a1
    ((x2min, y2min), (x2max, y2max)) = normalize a2
    w = min x1max x2max - max x1min x2min + 1
    h = min y1max y2max - max y1min y2min + 1

areaSize (c1, c2) = (dx + 1) * (dy + 1)
  where
    dx = abs $ fst c2 - fst c1
    dy = abs $ snd c2 - snd c1

part1 :: [Coord] -> Int
part1 coords =
  maximum [areaSize (c1, c2) | c1 <- coords, c2 <- coords, c1 /= c2]

uniqSorted :: Ord a => [a] -> [a]
uniqSorted = map head . group . sort

compressAxis :: [Coord] -> ([Int], [Int])
compressAxis coords = (xAll, yAll)
  where
    xCoords = uniqSorted (map fst coords)
    yCoords = uniqSorted (map snd coords)
    xAxes   = uniqSorted (concatMap (\x -> [x-1,x,x+1]) xCoords)
    yAxes   = uniqSorted (concatMap (\y -> [y-1,y,y+1]) yCoords)
    xMin = minimum xAxes - 1
    xMax = maximum xAxes + 1
    yMin = minimum yAxes - 1
    yMax = maximum yAxes + 1
    xAll = xMin : xAxes ++ [xMax]
    yAll = yMin : yAxes ++ [yMax]

-- Some assumptions made here: no zero-length edges, axis-aligned edges
boundaryWalk (x1, y1) (x2, y2) =
  [(x1 + i * xStep, y1 + i * yStep) | i <- [0 .. steps]]
  where
    (dx, dy) = (x2 - x1, y2 - y1)
    steps = max (abs dx) (abs dy)
    (xStep, yStep) = (dx `div` steps, dy `div` steps)

part2 coords = maxPair compressedCoords compressedArea'
  where
    n = length compressedCoords
    (xAll, yAll) = compressAxis coords
    xAllv = U.fromList xAll
    yAllv = U.fromList yAll

    xMap = Map.fromList $ zip xAll [0 ..]
    yMap = Map.fromList $ zip yAll [0 ..]
    compressedCoords =
      [ (Map.findWithDefault (-1) x xMap, Map.findWithDefault (-1) y yMap)
      | (x, y) <- coords
      ]
    compressedArea' c1 c2  = if rectSum h ps c1 c2 == 0 then area else 0
      where
        ((c1x, c1y), (c2x, c2y)) = (c1, c2)
        x1 = xAllv U.! min c1x c2x
        x2 = xAllv U.! max c1x c2x
        y1 = yAllv U.! min c1y c2y
        y2 = yAllv U.! max c1y c2y
        area = (x2 - x1 + 1) * (y2 - y1 + 1)

    dxs = U.zipWith (-) (U.tail xAllv) xAllv
    dys = U.zipWith (-) (U.tail yAllv) yAllv

    (w, h) = (U.length dxs, U.length dys)

    blockAreasVec :: U.Vector Int
    blockAreasVec =
      U.fromList
        [ dx * dy
        | (dx, i) <- U.toList (U.indexed dxs)
        , (dy, j) <- U.toList (U.indexed dys)
        , IS.notMember (pIdx (i, j)) outerArea
        ]

    boundarySet =
      IS.fromList $ map pIdx $
      concat
        [ boundaryWalk c1 c2
        | (c1, c2) <- zip compressedCoords (tail $ cycle compressedCoords)
        ]
    pIdx (x, y) = x * h + y
    outerArea = floodFill (0, 0)
    floodFill start = go IS.empty [start]
      where
        go visited [] = visited
        go visited (p:ps)
          | IS.member (pIdx p) visited = go visited ps
          | otherwise =
            let (x, y) = p
                neighbors =
                  [ (xNew, yNew) | 
                    (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                    not (IS.member (pIdx (x + dx, y + dy)) boundarySet),
                    (xNew, yNew) <- [(x + dx, y + dy)],
                    xNew >= 0 && xNew <= w,
                    yNew >= 0 && yNew <= h
                  ]
             in go (IS.insert (pIdx p) visited) (neighbors ++ ps)

    badArea :: U.Vector Int
    badArea =
      U.generate (w * h) $ \p ->
        let i = p `div` h
            j = p - i * h
         in if IS.member (pIdx (i, j)) outerArea
              then (dxs U.! i) * (dys U.! j)
              else 0

    buildPrefix :: Int -> Int -> U.Vector Int -> U.Vector Int
    buildPrefix w h bad =
      runST $ do
        let psH = h + 1
            psW = w + 1
            psSize = psW * psH
            psIdx i j = i * psH + j
        mps <- UM.replicate psSize 0
        forM_ [1 .. w] $ \i -> do
          let base = i * psH
              prev = (i - 1) * psH
              badBase = (i - 1) * h
          let go !j !run
                | j > h = pure ()
                | otherwise = do
                  let cell = bad U.! (badBase + (j - 1))
                      run' = run + cell
                  up <- UM.read mps (prev + j)
                  UM.write mps (base + j) (up + run')
                  go (j + 1) run'
          go 1 0
        U.unsafeFreeze mps
    ps = buildPrefix w h badArea

    rectSum :: Int -> U.Vector Int -> (Int, Int) -> (Int, Int) -> Int
    rectSum h ps (i0, j0) (i1, j1) =
      let psH = h + 1
          psIdx i j = i * psH + j
          x1 = i0
          y1 = j0
          x2 = i1 + 1
          y2 = j1 + 1
       in ps U.! psIdx x2 y2 - ps U.! psIdx x1 y2 - ps U.! psIdx x2 y1 +
          ps U.! psIdx x1 y1

    maxPair :: [Coord] -> (Coord -> Coord -> Int) -> Int
    maxPair pts f = goOuter pts 0
      where
        goOuter :: [Coord] -> Int -> Int
        goOuter [] !best = best
        goOuter (p:ps) !best =
          let !best' = goInner p ps best
          in goOuter ps best'

        goInner :: Coord -> [Coord] -> Int -> Int
        goInner _ [] !best = best
        goInner p (q:qs) !best =
          let !v = f p q
              !best' = max v best
          in goInner p qs best'

main :: IO ()
main = do
  test_input <- readFile "data/day9.txt"
  let test_data = parseInput test_input
  print $ part1 test_data
  print $ part2 test_data
