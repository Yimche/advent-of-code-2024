module Two where

import System.IO
import Control.Concurrent
import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

toInt :: [String] -> [Int]
toInt = map read

isSafe :: [Int] -> Bool
isSafe list = case list of
  []      -> True
  [x]     -> True
  x:y:xs
    | (x - y) > 0 && (x - y) <= 3   -> isSafeDecreasing (y:xs)
    | (x - y) < 0 && (x - y) >= -3  -> isSafeIncreasing (y:xs)
    | otherwise                     -> False

isSafeIncreasing :: [Int] -> Bool 
isSafeIncreasing list = case list of
  []      -> True
  [x]     -> True
  x:y:xs  -> (x - y) >= -3 && (x - y) < 0 && isSafeIncreasing (y:xs)

isSafeDecreasing :: [Int] -> Bool
isSafeDecreasing list = case list of
  []      -> True
  [x]     -> True
  x:y:xs  -> (x - y) <= 3 && (x - y) > 0 && isSafeDecreasing (y:xs)

main :: IO()
main = do 
  handle <- openFile "input" ReadMode  
  contents <- hGetContents handle  
  let linesList = lines contents
  let lineNums = map (toInt . words) linesList
  let safeList = map isSafe lineNums

  print $ count True safeList

  hClose handle

