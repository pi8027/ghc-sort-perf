{-# LANGUAGE NumericUnderscores #-}
module Main where

import Test.Tasty.Bench ( bench, bgroup, defaultMain, nf, Benchmark)
import Test.Tasty.QuickCheck
import System.Random (randomRIO)

import qualified Sorts.New3WM
import qualified Sorts.Old

import Control.Monad (replicateM)
import Control.DeepSeq (NFData)

import Test.Tasty.Providers (TestTree)
import Data.Ord (comparing)

main :: IO ()
main = do
  tData <- mapM benchmark sizes
  defaultMain $ testCorrect : testStable : tData

sorts :: Ord a => (a -> a -> Ordering) -> [[a] -> [a]]
sorts cmp = ($ cmp) <$> [Sorts.Old.sortBy, Sorts.New3WM.sortBy]

testCorrect :: TestTree
testCorrect = testProperty "correct" $
  \d -> allEq $ map (\f -> f (d :: [Int])) (sorts compare)

testStable :: TestTree
testStable = testProperty "stable" $
  \d -> allEq $ map (\f -> f $ zip (d :: [Int]) [(0 :: Int)..]) (sorts (comparing fst))

sizes :: [Int]
sizes = [ 100, 10_000, 100_000, 1_000_000 ]

benchmark :: Int -> IO Benchmark
benchmark size = do
  dataN <- randoms size 10
  let name n = concat [n, " - ", show size]
      random    = mk (name "Random") dataN map
      expensive = mk (name "Expensive-Random") dataN (\f -> map (f . map (\x -> replicate 500 0 ++ [x])))
      sorted    = mk (name "Sorted") [1..size] id
      reversed  = mk (name "Reverse-Sorted") (reverse [1..size]) id
  pure $ bgroup "sort" [random, sorted, reversed]

mk :: (Ord a, NFData b) => String -> c -> (([a] -> [a]) -> c -> b) -> Benchmark
mk name dataN f = bgroup name 
  [ bench "original" $ foo Sorts.Old.sort
  , bench "3 way merge" $ foo Sorts.New3WM.sort
  ]
  where foo g = nf (f g) dataN

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (== x) xs

randoms :: Int -> Int -> IO [[Int]]
randoms n m = replicateM m $ replicateM n $ randomRIO (0, 10_000)
