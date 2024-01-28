{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sort" #-}
module Main where

import Test.Tasty.Bench ( bench, bgroup, nf, Benchmark, bcompare, defaultMain, Benchmarkable )
import Test.Tasty.QuickCheck
import System.Random (randomRIO)

import qualified Sorts.New3WM    as N3
import qualified Sorts.New3WMOpt as N3O
import qualified Sorts.New4WM    as N4
import qualified Sorts.Old       as Old


import Control.Monad (replicateM)
import Control.DeepSeq (NFData)

import Test.Tasty.Providers (TestTree, singleTest)
import Data.List hiding (sort, sortBy)
import Test.Tasty (testGroup)
import Data.Semigroup (Arg(..))

import ComparisonProvider (ComparisonTest(..))
import Data.Data (Typeable)

baseline :: String
baseline = "Old"

type ComparisonFunction a = a -> a -> Ordering


sorts :: Ord a => [(String, ComparisonFunction a -> [a] -> [a])]
sorts =
  [ (baseline, Old.sortBy)
  , ("3 Way Merge", N3.sortBy)
  , ("3 Way Merge Optimization", N3O.sortBy)
  , ("4 Way Merge", N4.sortBy) ]

main :: IO ()
main = do
  tData <- mapM benchmark sizes
  defaultMain (testAll : tData)

testAll :: TestTree
testAll = testGroup "List tests"
  [ makeTest "correctness" (isCorrect @Int)
  , makeTest "stability" (isStable @Int) ]

makeTest :: (Ord a, Arbitrary b, Show b) => String -> (([a] -> [a]) -> [b] -> Property) -> TestTree
makeTest name f = testGroup name $ map (\(n, sortBy) -> testProperty n $ f (sortBy compare)) sorts

isStable :: Ord a => ([Arg a Int] -> [Arg a Int]) -> [a] -> Property
isStable sort xs = let result = sort (zipWith Arg xs [0..])
  in property $ isAscending result

isAscending :: (Eq a, Ord b) => [Arg a b] -> Bool
isAscending [] = True
isAscending [_] = True
isAscending ((Arg x1 i1) : a@((Arg x2 i2) : _))
  | x1 == x2  = i1 < i2 && isAscending a
  | otherwise = isAscending a

-- isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Bool
isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Property
isCorrect sort xs = let res = sort xs in isSorted res .&&. sameElems res xs
  where sameElems x y = null (x \\ y) && null (y \\ x)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

sizes :: [Int]
sizes = [0 .. 10] ++ [ 100, 1000, 10_000, 100_000, 1_000_000 ]

benchmark :: Int -> IO Benchmark
benchmark size = do
  dataN <- randoms size
  let randomSort  = bgroup "sort" (makeBench dataN id)
      minimumElem = bgroup "min by sort" (makeBench dataN (take 1))
      comparisons = testGroup "comparisons" (makeComps dataN)
  pure $ bgroup (show size ++ " Elements") [randomSort, minimumElem, comparisons]

makeBench :: (NFData b, Ord a) => [a] -> ([a] -> b) -> [Benchmark]
makeBench _data f = forSorts (\name sortBy -> compBench name $ nf (f . sortBy compare) _data)

makeComps :: (Ord a, NFData a, Typeable a) => [a] -> [TestTree]
makeComps _data = forSorts (\name sortBy -> singleTest name (ComparisonTest _data sortBy))

forSorts :: Ord a => (String -> (ComparisonFunction a -> [a] -> [a]) -> b) -> [b]
forSorts f = map (uncurry f) sorts

compBench :: String -> Benchmarkable -> Benchmark
-- compBench str
--  | str == baseline = bench str
--  | otherwise       = bcompare baseline . bench str
compBench = bench


randoms :: Int -> IO [Int]
randoms n = replicateM n $ randomRIO (0, 10_000)
