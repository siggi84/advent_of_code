module MyLib
  ( someFunc
  , safeHead
  , safeLast
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (a:_) = Just a

safeLast :: [Int] -> Maybe Int
safeLast [] = Nothing
safeLast a = Just (last a)
