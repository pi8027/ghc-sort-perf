module Main where

import Criterion.Main ( defaultMain, bgroup, nf, bench, Benchmark )
import qualified Data.List (sort)
import System.Random (randomRIO)

import qualified Sorts.New3WM
import qualified Sorts.New4WM
import qualified Sorts.New5WM
import qualified Sorts.New5WMBinPart
import Control.Monad (replicateM)

main :: IO ()
main = do
  tData <- mapM benchmark sizes
  defaultMain [ bgroup "sort" tData]

sizes :: [Int]
sizes = [10, 100, 10000, 100000, 500000, 1000000, 2500000]


benchmark :: Int -> IO Benchmark
benchmark n = do
  dataN <- randoms n
  pure $ bgroup ("Random - " ++ show n)
    [ bench "original" $ nf Data.List.sort dataN
    , bench "3 way merge" $ nf Sorts.New3WM.sort dataN
    , bench "4 way merge" $ nf Sorts.New4WM.sort dataN
    , bench "5 way merge no intermediate" $ nf Sorts.New5WMBinPart.sort dataN
    , bench "5 way merge" $ nf Sorts.New5WM.sort dataN
    ]

randoms :: Int -> IO [Int]
randoms n = replicateM n $ randomRIO (0, 100000)
