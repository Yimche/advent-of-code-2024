module One where

import System.IO
import Control.Concurrent
import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

toInt :: [String] -> [Int]
toInt = map read

main :: IO()
main = do 
  handle <- openFile "input" ReadMode  
  contents <- hGetContents handle  
  let linesList = lines contents
  let lineNums = map (toInt . words) linesList

  let lefts = sort $ map head lineNums
  let rights = sort $ map (!!1)  lineNums

  let tups = [ (l, r) | (l, r) <- zip lefts rights]

  let difs = map (\x@(l, r) -> max l r - min l r) tups
  
  print $ sum difs
  
  let similarities = map (`count`  rights) lefts

  let scores = zipWith (*) similarities lefts
  print $ sum scores

  hClose handle
