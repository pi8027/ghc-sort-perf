{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Tasty.Bench ( bench, bgroup, nf, Benchmark, bcompare, defaultMain )
import Test.Tasty.QuickCheck
import System.Random (randomRIO)

import qualified Sorts.New3WM    as N3
import qualified Sorts.New3WMOpt as N3O
import qualified Sorts.New4WM    as N4
import qualified Sorts.Old       as Old


import Control.Monad (replicateM)
import Control.DeepSeq (NFData)

import Test.Tasty.Providers (TestTree)
import Data.List ((\\))
import Test.Tasty (testGroup)
import Data.Semigroup (Arg(..))


baseline :: String
baseline = "Old"

sorts :: Ord a => [(String, [a] -> [a])]
sorts =
  [ (baseline, Old.sort)
  , ("3 Way Merge", N3.sort)
  , ("3 Way Merge Optimization", N3O.sort)
  , ("4 Way Merge", N4.sort) ]

main :: IO ()
main = do
  tData <- mapM benchmark sizes
  defaultMain (testAll : tData)

testAll :: TestTree
testAll = testGroup "List tests"
  [ makeTest "correctness" (isCorrect @Int)
  , makeTest "stability" (isStable @Int) ]

makeTest :: (Ord a, Arbitrary b, Show b) => String -> (([a] -> [a]) -> [b] -> Property) -> TestTree
makeTest name f = testGroup name $ map (\(n, alg) -> testProperty n (f alg)) sorts

isStable :: Ord a => ([Arg a Int] -> [Arg a Int]) -> [a] -> Property
isStable alg xs = let result = alg (zipWith Arg xs [0..])
  in property $ isAscending result

isAscending :: (Eq a, Ord b) => [Arg a b] -> Bool
isAscending [] = True
isAscending [_] = True
isAscending ((Arg x1 i1) : a@((Arg x2 i2) : _))
  | x1 == x2  = i1 < i2 && isAscending a
  | otherwise = isAscending a

-- isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Bool
isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Property
isCorrect alg xs = let res = alg xs in isSorted res .&&. sameElems res xs
  where sameElems x y = null (x \\ y) && null (y \\ x)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

sizes :: [Int]
sizes = [ 10_000, 100_000, 1_000_000, 5_000_000 ]

benchmark :: Int -> IO Benchmark
benchmark size = do
  dataN <- randoms size
  let name n = concat [n, " - ", show size]
      random    = mk' (name "Random") dataN
      sorted    = mk' (name "Sorted") [1..size]
  pure $ bgroup "sort" [random]


mk' :: (NFData a, Ord a) => String -> [a] -> Benchmark
mk' name _data = bgroup name $ map (uncurry $ makeBench _data) sorts

makeBench :: NFData b => a -> String -> (a -> b) -> Benchmark
makeBench _data name alg = {- comp name $-} bench name (nf alg _data)

-- comp :: String -> Benchmark -> Benchmark
-- comp name
--  | name /= baseline = bcompare baseline
--  | otherwise        = id

randoms :: Int -> IO [Int]
randoms n = replicateM n $ randomRIO (0, 10_000)
